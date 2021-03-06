(in-package :clisprolog)

(defun lower-char-p (char)
  (char<= #\a char #\z))

(defun upper-char-p (char)
  (char<= #\A char #\Z))

(defun number-char-p (char)
  (char<= #\0 char #\9))

(defun empty-str-p (str)
  (zerop (length str)))

(defun head-char (str)
  (aref str 0))

(defun str-head (str)
  (subseq str 0 1))

(defun str-tail (str)
  (subseq str 1))

(defun get-keysym (name)
  (intern name 'keyword))

(defmacro if-not (test-form else-form then-form)
  `(if ,test-form ,then-form ,else-form))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro push-back (obj place)
  (with-gensyms (gprev)
    `(let ((,gprev ,place))
       (setf ,place (append ,gprev (list ,obj))))))

(defmacro string-case (str &rest body)
  `(cond ,@(mapcar (lambda (x) `((string= ,str ,(car x)) ,@(cdr x)))
                   body)))


(defmacro define-class (class-name parent  &rest res)
  `(defclass ,class-name ,parent
     ,(mapcar (lambda (lis)
		(if (listp lis)
		    (apply(lambda (x &optional (y nil) (z x))
			    `(,x :initarg 
				 ,(intern (symbol-name x) "KEYWORD") 
				 :initform ,y :accessor ,z))
			  lis)
		    ((lambda (x) 
		       `(,x :initarg 
			    ,(intern (symbol-name x) "KEYWORD") 
			    :initform nil :accessor ,x))
		     lis)))
	      res)))
