(in-package :on-lisp.09)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 9 - Variable Capture

;; p. 127

;; version 1
#+nil
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

;; version 2
#+nil
(defmacro for ((var start stop) &body body)
  `(do ((b (lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

;; p. 129

;; version 3 -- correct
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
