(in-package :clisprolog)

(defvar *operators* '("+" "-" "*" "/" ":-"
                      "=" "==" "===" "<" ">"
                      "." "(" ")" "," "[" "]" "|"))

(defun operand-char-p (char)
  (or (lower-char-p char)
      (upper-char-p char)
      (number-char-p char)
      (char= char #\_)))

(defun begin-op-p (str)
  (labels ((rec (str op-list)
             (cond ((null op-list) nil)
                   ((and (search (car op-list) str)
                         (zerop (search (car op-list) str))) t)
                   (t (rec str (cdr op-list))))))
    (rec str *operators*)))

(defmacro go-down-tokenize (rec str)
  (with-gensyms (gsucc grest)
    `(multiple-value-bind (,gsucc ,grest) (,rec (str-tail ,str))
       (values (concatenate
                'string (str-head ,str) ,gsucc)
               ,grest))))


(defun get-atom (str)
  (cond ((empty-str-p str) (values str ""))
        ((not (operand-char-p (head-char str)))
         (values "" str))
        (t (go-down-tokenize get-atom str))))

(defun get-quote-atom (str)
  (labels ((rec (str)
             (cond ((empty-str-p str)
                    (error "parse error: invalid quotation"))
                   ((char= (head-char str) #\')
                    (values (str-head str)
                            (str-tail str)))
                   (t (go-down-tokenize rec str)))))
    (go-down-tokenize rec str)))


(defun get-var (str)
  (labels ((rec (str)
             (cond ((empty-str-p str) (values str ""))
                   ((not (operand-char-p (head-char str)))
                    (values "" str))
                   (t (go-down-tokenize rec str)))))
    (go-down-tokenize rec str)))

(defun get-number (str)
  (let ((haspoint nil))
    (labels ((rec (str)
               (cond ((empty-str-p str)
                      (values str ""))
                     ((char= (head-char str) #\.)
                      (cond ((or (empty-str-p (str-tail str))
                                 (not (number-char-p
                                       (head-char (str-tail str)))))
                             (values "" str))
                            (haspoint
                             (error "parse error: multiple decimal points"))
                            (t (setf haspoint t)
                               (go-down-tokenize rec str))))
                     ((number-char-p (head-char str))
                      (go-down-tokenize rec str))
                     (t (values "" str)))))
      (rec str))))

(defun get-op (str)
  (labels ((rec (str op-list)
               (cond ((null op-list)
                      (error "lexer error: operator not found ~A"
                             str))
                     ((and (search (car op-list) str)
                           (zerop (search (car op-list) str)))
                      (values (car op-list)
                              (subseq str (length (car op-list)))))
                     (t (rec str (cdr op-list))))))
    (rec str *operators*)))

(defun string-to-token (str)
  (setf str (string-trim " " str))
  (unless (empty-str-p str)
    (let ((head (head-char str)))
      (cond ((lower-char-p head)
             (multiple-value-bind (tok rest) (get-atom str)
               (cons tok (string-to-token rest))))
            ((char= head #\')
             (multiple-value-bind (tok rest) (get-quote-atom str)
               (cons tok (string-to-token rest))))
            ((or (upper-char-p head)
                 (char= head #\_))
             (multiple-value-bind (tok rest) (get-var str)
               (cons tok (string-to-token rest))))
            ((number-char-p head)
             (multiple-value-bind (tok rest) (get-number str)
                 (cons tok (string-to-token rest))))
            ((begin-op-p str)
             (multiple-value-bind (tok rest) (get-op str)
               (cons tok (string-to-token rest))))
            (t (error "read error ~A~%" str))))))

(defun lexer (path)
  (with-open-file (in path)
    (iter (for line in-stream in using #'read-line)
          ;;(collect (string-to-token line)))))
          (nconcing (string-to-token line)))))
