(in-package :clisprolog)

(let ((lexer nil)
      (hold nil))
  (defun make-reader (path)
    (setf lexer (make-lexer path))
    (lambda () (get-term :global)))

  (defun make-reader-from-string (str)
    (setf lexer (make-lexer-from-string str))
    (lambda () (get-term :global)))

  (defun read-token ()
    (if (null hold)
        (funcall lexer) (pop hold)))
  
  (defun get-term (context)
    (let ((token (read-token)))
      (if (null token)
          (if (eq context :global)
              nil (error "paren unmatch"))
          (case (second token)
            (:lparen
             (make-instance 'prolog-paren
                            :value (read-block :paren)))
            (:rparen
             (if (eq context :paren)
                 (progn (push token hold) nil)
                 (error "paren unmatch")))
            (:lbracket
             (make-instance 'prolog-list
                            :value (read-block :list)))
            (:rbracket
             (if (eq context :list)
                 (progn (push token hold) nil)
                 (error "paren unmatch")))
            (:atom
             (read-atom token))
            (:comma
             (if (eq context :global)
                 (make-instance 'prolog-op :name ",")
                 (error "multiple separator")))
            (otherwise
             (make-value token))))))

  (defun read-block (context)
    (let ((term (get-term context))
          (next (read-token)))
      (case (second next)
        (:rparen
         (if (eq context :paren)
             (if (null term)
                 nil (list term))
             (error "paren unmatch")))
        (:rbracket
         (if (eq context :list)
             (if (null term)
                 nil (list term))
             (error "paren unmatch")))
        (:comma
         (cons term (read-block context)))
        (otherwise
         (push next hold)
         (cons term (read-block context))))))

  (defun read-atom (token)
    (let ((name (first token))
          (next (read-token)))
      (if (or (null next)
              (not (eq (second next) :lparen)))
          (progn
            (push next hold)
            (make-instance 'prolog-atom :value name))
          (make-instance 'prolog-pred
                         :name name :args (read-block :paren)))))

  (defun make-value (token)
    (let ((tok (first token))
          (tag (second token)))
      (case tag
        (:period (make-instance 'prolog-period))
        (:semicolon (make-instance 'prolog-op :name ";"))
        (:pipe (make-instance 'prolog-op :name "|"))
        (:number (make-instance 'prolog-num
                                :value (read-from-string tok)))
        (:variable (make-instance 'prolog-var :name tok))
        (:operator (make-instance 'prolog-op :name tok))))))
  

(defun test-read (path)
  (mapc (lambda (val) (print (prolog-value-str val)))
        (reader (lexer path) :global)))


(defun make-terms (path)
  (let ((reader (make-reader path)))
    (labels ((rec ()
               (let ((term (funcall reader)))
                 (when term
                   (cons term (rec))))))
      (rec))))
  
(defun make-terms-from-string (str)
  (let ((reader (make-reader-from-string str)))
    (labels ((rec ()
               (let ((term (funcall reader)))
                 (when term
                   (cons term (rec))))))
      (rec))))
