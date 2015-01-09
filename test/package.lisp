(defpackage :on-lisp-test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.02
        :on-lisp.03
        :on-lisp.04
        :on-lisp.05
        :on-lisp.06
        :on-lisp.07
        :on-lisp.08
        :on-lisp.09
        :on-lisp.10
        :on-lisp.11
        :on-lisp.12
        :on-lisp.13
        :on-lisp.14
        :on-lisp.15
        :on-lisp.16
        :on-lisp.17
        :on-lisp.18
        :on-lisp.20
        :on-lisp.21
        :on-lisp.22
        :on-lisp.23)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.19.interpreted.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.19.interpreted)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.19.compiled.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.19.compiled)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.24.interpreted.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.24.interpreted)
  (:import-from :on-lisp.18
                :gensym?)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.24.compiled.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.24.compiled)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.24.compiled-plus.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.24.compiled-plus)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.25.v1.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.25.v1)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.25.v2.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.25.v2)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.25.v3.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.25.v3)
  (:export
   #:run-all-tests))

(defpackage :on-lisp.25.v4.test
  (:use :cl :stefil :lisp-unit2 :named-readtables
        :on-lisp.25.v4)
  (:export
   #:run-all-tests))

(in-package :on-lisp-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-readtable :on-lisp-test)
    (defreadtable :on-lisp-test
      (:merge :on-lisp.17)
      
      ;; extra read-macro for convenience, not defined in the book
      (:dispatch-macro-char #\# #\`
                            ;; copy-tree, for when it's natural to express a list
                            ;; as a quoted form but you  can't risk someone else
                            ;; mutating it (which pg does in the book several times)
                            (lambda (stream subchar arg)
                              (declare (ignore subchar arg))
                              `(copy-tree (quote ,(read stream t nil t))))))))
