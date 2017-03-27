(in-package :clisprolog)

(define-class prolog-value ())
(defgeneric prolog-value-str (value))

(define-class prolog-pred (prolog-value)
  name args)
(defmethod prolog-value-str ((pred prolog-pred))
  (format nil "Pred(~A(~{~a~^,~}))" (name pred) (mapcar #'prolog-value-str
                                                   (args pred))))

(define-class prolog-num (prolog-value)
  value)
(defmethod prolog-value-str ((num prolog-num))
  (format nil "Num(~A)" (value num)))

(define-class prolog-atom (prolog-value)
  name)
(defmethod prolog-value-str ((atom prolog-atom))
  (format nil "Atom(~A)" (name atom)))

(define-class prolog-var (prolog-value)
  name)
(defmethod prolog-value-str ((var prolog-var))
  (format nil "Var(~A)" (name var)))

(define-class prolog-op (prolog-value)
  name rank assoc-type)
(defmethod prolog-value-str ((op prolog-op))
  (format nil "Op(~A)" (name op)))

(define-class prolog-list (prolog-value)
  value)
(defmethod prolog-value-str ((lst prolog-list))
  (format nil "List(~{~a~^,~})" (mapcar #'prolog-value-str
                                 (value lst))))

(define-class prolog-paren (prolog-value)
  value)
(defmethod prolog-value-str ((paren prolog-paren))
  (format nil "Paren(~A)" (mapcar #'prolog-value-str
                                  (value paren))))

(define-class prolog-period (prolog-value))
(defmethod prolog-value-str ((period prolog-period))
  (format nil "Period"))


(defun operand-p (term)
  (not (find (type-of term)
             '(prolog-op prolog-period)
             :test #'eq)))
