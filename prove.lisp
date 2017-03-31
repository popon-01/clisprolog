(in-package :clisprolog)

(defvar *fact/rule-table* (make-hash-table))

(defun exec-ast-list (ast-list)
  (unless (null ast-list)
    (let ((ast (car ast-list)))
      (cond ((eq (type-of ast) 'prolog-pred)
             (push-back ast (gethash (get-keysym (name ast))
                                     *fact/rule-table*)))
            ((and (listp ast)
                  (string= (name (first ast)) ":-"))
             (push-back ast (gethash (get-keysym (name (second ast)))
                                     *fact/rule-table*))))
      (exec-ast-list (cdr ast-list)))))

(defun set-variant (ast)
  (let ((var-table (make-hash-table)))
    (labels ((rec (ast)
               (cond ((listp ast)
                      (mapcar #'rec ast))
                     ((eq (type-of ast) 'prolog-pred)
                      (make-instance 'prolog-pred
                                     :name (name ast)
                                     :args (mapcar #'rec (args ast))))
                     ((eq (type-of ast) 'prolog-var)
                      (if (string= (name ast) "_")
                          (make-instance 'prolog-var
                                         :name (string (gensym)))
                          (aif (gethash (get-keysym (name ast)) var-table)
                               (make-instance 'prolog-var :name it)
                               (let ((newsym (string (gensym))))
                                 (setf (gethash (get-keysym (name ast))
                                                var-table)
                                       newsym)
                                 (make-instance 'prolog-var :name newsym)))))
                     (t ast))))
      (rec ast))))


(defun gen-ast-prove (ast)
  (cond ((and (listp ast) (string= (name (first ast)) ","))
         (gen-comma-prove ast))
        ((and (listp ast) (string= (name (first ast)) ","))
         (gen-semicolon-prove ast))
        ((and (listp ast) (string= (name (first ast)) "is"))
         (gen-is-prove ast))
        ((and (listp ast) (find (name (first ast)) '("<" ">" "=\=" "=:=")
                                :test #'string=))
         (gen-comp-op-prove ast))
        ((eq (type-of ast) 'prolog-pred)
         (gen-pred-prove ast))))

(defun calc-ast-num (ast)
  (cond ((eq (type-of ast) 'prolog-num) ast)
        ((and (listp ast)
              (eq (type-of (first ast)) 'prolog-op)
              (find (name (first ast))
                    '("+" "-" "*" "/") :test #'string=))
         (let ((arg1 (value (calc-ast-num (second ast))))
               (arg2 (value (calc-ast-num (third ast)))))
           (string-case (name (first ast))
                        ("+"
                         (make-instance 'prolog-num
                                        :value (+ arg1 arg2)))
                        ("-"
                         (make-instance 'prolog-num
                                 :value (- arg1 arg2)))
                        ("*"
                         (make-instance 'prolog-num
                                        :value (* arg1 arg2)))
                        ("/"
                         (make-instance 'prolog-num
                                        :value (float (/ arg1 arg2)))))))
        (t (error "calculation failed."))))

(defun gen-is-prove (ast)
  (let ((called nil))
    (lambda ()
      (if called
          (values nil nil)
          (let ((arg2 (calc-ast-num (third ast))))
            (setf called t)
            (unify (second ast) arg2))))))

(defun gen-comp-op-prove (ast)
  (let ((called nil))
    (lambda ()
      (if called
          (values nil nil)
          (let ((arg1 (value (calc-ast-num (second ast))))
                (arg2 (value (calc-ast-num (third ast)))))
            (setf called t)
            (string-case (name (first ast))
                         ("<"
                          (values nil (< arg1 arg2)))
                         (">"
                          (values nil (> arg1 arg2)))
                         ("=:="
                          (values nil (not (= arg1 arg2))))
                         ("=\="
                          (values nil (not (= arg1 arg2))))))))))

  

(defun gen-comma-prove (ast)
  (let ((arg1-gen (gen-ast-prove (second ast)))
        (rest nil)
        (current-bind nil))
    (labels ((rec ()
               (if-not (null rest)
                       (multiple-value-bind (res find) (funcall rest)
                         (if find
                             (values (append current-bind res) t)
                             (progn (setf rest nil
                                          current-bind nil)
                                    (rec))))
                       (multiple-value-bind (res1 find1)
                           (funcall arg1-gen)
                         (if-not find1
                                 (values nil nil)
                                 (let ((next (apply-bind (third ast) res1)))
                                   (setf rest (gen-ast-prove next)
                                         current-bind res1)
                                   (multiple-value-bind (res2 find2)
                                       (funcall rest)
                                     (if find2
                                         (values (append current-bind res2) t)
                                         (progn (setf rest nil) (rec))))))))))
      #'rec)))

(defun gen-semicolon-prove (ast)
  (let ((fact/rule (cdr ast))
        (gen nil))
    (labels ((rec ()
               (cond ((null gen)
                      (multiple-value-bind (res find) (funcall gen)
                        (if find
                            (values res t)
                            (progn (setf gen nil) (rec)))))
                     ((null fact/rule) (values nil nil))
                     (t (setf gen (gen-ast-prove (pop fact/rule)))
                        (rec)))))
      #'rec)))


(defun gather-res (pred bind)
  (labels ((rec (args acc)
             (if (null args)
                 acc
                 (let ((arg (car args)))
                   (if (eq (type-of arg) 'prolog-var)
                       (rec (cdr args) (cons (cons (get-keysym (name arg))
                                                   (get-bind-value arg bind))
                                             acc))
                       (rec (cdr args) acc))))))
    (rec (args pred) nil)))

(defun gen-pred-prove (pred)
  (let ((fact/rule (gethash (get-keysym (name pred))
                            *fact/rule-table*))
        (rest nil)
        (current-bind nil))
    (labels ((rec ()
               (cond ((not (null rest))
                      (multiple-value-bind (res find) (funcall rest)
                        (if find
                            (let ((bind (append current-bind res)))
                              (values (gather-res pred bind) t))
                            (progn (setf rest nil
                                         current-bind nil)
                                   (rec)))))
                     ((null fact/rule) (values nil nil))
                     ((listp (car fact/rule)) 
                      (let ((rule (set-variant (pop fact/rule))))
                        (multiple-value-bind (bind unifiable)
                            (unify pred (second rule))
                          (if-not unifiable
                                  (rec)
                                  (let ((goals (apply-bind (third rule) bind)))
                                    (setf rest (gen-ast-prove goals)
                                          current-bind bind)
                                    (multiple-value-bind (res find)
                                        (funcall rest)
                                      (if find
                                          (let ((bind (append current-bind res)))
                                            (values (gather-res pred bind) t))
                                          (progn (setf rest nil
                                                       current-bind nil)
                                                 (rec)))))))))
                     ((eq (type-of (car fact/rule)) 'prolog-pred)
                      (multiple-value-bind (res find)
                          (unify pred (pop fact/rule))
                        (if find (values (gather-res pred res) find) (rec)))))))
      #'rec)))

