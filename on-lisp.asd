(require "asdf")

(defpackage #:on-lisp-system
  (:use #:cl :asdf))
(in-package :on-lisp-system)

(defsystem :on-lisp
  :name "on-lisp"
  :components
  ((:static-file "on-lisp.asd")
   (:module :src
            :components
            ((:file "package")
             (:file "on-lisp" :depends-on ("package")))))
  :depends-on ())

(defsystem :on-lisp-test
  :depends-on (:on-lisp :stefil :macroexpand-dammit)
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "on-lisp-test" :depends-on ("package"))))))
