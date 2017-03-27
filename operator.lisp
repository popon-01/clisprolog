(in-package :clisprolog)

(defmacro make-opdef-list (&rest rankdefs)
  `(list
    ,@(iterate
       (for opdef on rankdefs by (lambda (lst) (nthcdr 3 lst)))
       (collect 
           `(make-instance 'prolog-op
                           :name ,(first opdef)
                           :rank ,(second opdef)
                           :assoc-type  ,(third opdef))))))

(defvar *op-defs*
  (make-opdef-list ":-" 1200 :xfx "?-" 1200 :fx  ";"  1100 :xfy
                   ","  1000 :xfy "="   700 :xfx "is"  700 :xfx
                   "<"   700 :xfx ">"   700 :xfx "=="  700 :xfx
                   "=\=" 700 :xfx "+"   500 :yfx "+"   500 :fx
                   "-"   500 :yfx "-"   500 :fx  "*"   400 :yfx
                   "/"   400 :yfx))


