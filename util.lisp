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
