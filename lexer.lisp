(in-package :clisprolog)

(defvar *operators* '("+" "-" "*" "/" ":-"
                      "=" "=:=" "=\=" "<" ">"
                      "." "(" ")" "," "[" "]" "|"))

(defun operand-char-p (char)
  (or (lower-char-p char)
      (upper-char-p char)
      (number-char-p char)
      (char= char #\_)))

(defun begin-op-p (char)
  (labels ((rec (op-list)
             (cond ((null op-list) nil)
                   ((and (search (string char) (car op-list))
                         (zerop (search (string char) (car op-list)))) t)
                   (t (rec (cdr op-list))))))
    (rec *operators*)))

(defun atom-info (atom)
  (if (string= atom "is")
      :operator :atom))

(defun op-info(op)
  (let ((special-op-list '("(" :lparen ")" :rparen
                           "[" :lbracket "]" :rbracket
                           "," :comma "." :period
                           "|" :pipe ";" :semicolon)))
    (labels ((rec (lst)
               (cond ((null lst) :operator)
                     ((string= op (first lst)) (second lst))
                     (t (rec (cddr lst))))))
      (rec special-op-list))))

(let ((in nil)
      (hold nil))
  (defun make-lexer (path)
    (setf in (open path))
    (lambda () (get-token)))

  (defun make-lexer-from-string (str)
    (setf in (make-string-input-stream str))
    (lambda () (get-token)))

  (defun read-head ()
    (if (null hold)
        (read-char in nil nil)
        (pop hold)))
  
  (defun get-symbol ()
    (let ((head (read-head)))
      (cond ((null head) (close in) "")
            ((not (operand-char-p head))
             (push head hold) "")
            (t (concatenate 'string (string head) (get-symbol))))))
  
  (defun get-quote-atom ()
    (labels ((rec ()    
               (let ((head (read-head)))
                 (cond ((null head)
                        (error "parse error: invalid quotation"))
                       ((char= head #\') (string head))
                       (t (concatenate 'string
                                       (string head) (rec)))))))
      (let ((head (read-head)))
        (assert (char= head #\'))
        (concatenate 'string (string head) (rec)))))
    
  (defun get-number ()
    (let ((haspoint nil))
      (labels
        ((rec ()
           (let ((head (read-head)))
             (cond ((null head) (close in) "")
                   ((char= head #\.)
                    (let ((head2 (read-head)))
                      (cond ((or (null head2)
                                 (not (number-char-p head2)))
                             (push head2 hold)
                             (push head  hold) "")
                            (haspoint
                             (error
                              "parse error: multiple decimal points"))
                            (t (setf haspoint t)
                               (push head2 hold)
                               (concatenate 'string
                                            (string head) (rec))))))
                   ((number-char-p head)
                    (concatenate 'string (string head) (rec)))
                   (t (push head hold) "")))))
        (rec))))
  
  (defun get-op ()
    (let ((head (read-head)))
      (labels
          ((rec (op-list)
             (cond ((null op-list)
                    (error "lexer error: operator not found ~A"
                           (string head)))
                   ((and (search (string head) (car op-list))
                         (zerop (search (string head) (car op-list))))
                    (iterate (repeat (1- (length (car op-list))))
                             (read-head))
                    (car op-list))
                   (t (rec (cdr op-list))))))
        (rec *operators*))))
    
  (defun get-token ()
    (if-not (open-stream-p in) nil
            (let ((head (read-head)))
              (if (null head)
                  (progn
                    (close in) nil)
                  (progn
                    (push head hold)
                    (cond ((lower-char-p head)
                           (let ((tok (get-symbol)))
                             (list tok (atom-info tok))))
                          ((char= head #\')
                           (list (get-quote-atom) :atom))
                          ((or (upper-char-p head)
                               (char= head #\_))
                           (list (get-symbol) :variable))
                          ((number-char-p head)
                           (list (get-number) :number))
                          ((begin-op-p head)
                           (let ((tok (get-op)))
                             (list tok (op-info tok))))
                          ((find head '(#\  #\newline))
                           (pop hold) (get-token))
                          (t (error "read error ~A~%" (string head))))))))))

(defun make-lexer-test (path)
  (let ((lex (make-lexer path)))
    (labels ((rec ()
               (let ((token (funcall lex)))
                 (when token
                   (print token)
                   (cons token (rec))))))
      (rec))))
  

