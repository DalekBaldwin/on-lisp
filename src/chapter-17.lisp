(in-package :on-lisp.17)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 17 - Read-Macros

;; p. 225

#+nil
(set-macro-character #\'
  #'(lambda (stream char)
      (list 'quote (read stream t nil t))))

;; note how abbreviation for quote cannot be defined as normal macro
;; keep this in mind when using nested macrolet with macroexpand...
#+nil
(defmacro q (obj)
  `(quote ,obj))

;; p. 226
(set-dispatch-macro-character #\# #\?
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    `(lambda (&rest ,(gensym))
       ,(read stream t nil t))))

;; p. 227
(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (let ((accum nil)
          (pair (read-delimited-list #\] stream t)))
      (do ((i (ceiling (car pair)) (1+ i)))
          ((> i (floor (cadr pair)))
           (list 'quote (nreverse accum)))
        (push i accum)))))

;; p. 228
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right (lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
      (lambda (stream char1 char2)
        (declare (ignore char1 char2))
        (apply fn
               (read-delimited-list right stream t))))))

#+nil
(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

;; p. 229
(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))
