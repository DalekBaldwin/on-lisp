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
#+nil
(set-dispatch-macro-character #\# #\?
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    `(lambda (&rest ,(gensym))
       ,(read stream t nil t))))

;;; Changed from anonymous lambda to |#?-reader| to support named-readtables
(defun |#?-reader| (stream char numarg)
  (declare (ignore char numarg))
  `(lambda (&rest ,(gensym))
     ,(read stream t nil t)))

;; p. 227
#+nil
(set-macro-character #\] (get-macro-character #\)))

#+nil
(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (let ((accum nil)
          (pair (read-delimited-list #\] stream t)))
      (do ((i (ceiling (car pair)) (1+ i)))
          ((> i (floor (cadr pair)))
           (list 'quote (nreverse accum)))
        (push i accum)))))

;;; Changed from anonymous lambda to |#[-reader| to support named-readtables
(defun |#[-reader| (stream char numarg)
  (declare (ignore char numarg))
  (let ((accum nil)
        (pair (read-delimited-list #\] stream t)))
    (do ((i (ceiling (car pair)) (1+ i)))
        ((> i (floor (cadr pair)))
         (list 'quote (nreverse accum)))
      (push i accum))))

;; p. 228
(defmacro defdelim# (left right params &body body)
  `(ddfn# ,left ,right (lambda ,params ,@body)))

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right (lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn# (left rigth fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
      (lambda (stream char)
        (declare (ignore char))
        (apply fn
               (read-delimited-list right stream t)))))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-macro-character left
      (lambda (stream char1 char2)
        (declare (ignore char1))
        (apply fn
               (read-delimited-list right stream t))))))

#+nil
(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

;; p. 229
#+nil
(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

;;; Changed from anonymous lambda to |#{-reader| to support named-readtables
(defun |#{-reader| (stream char numarg)
  (declare (ignore char numarg))
  `(fn (compose ,@(read-delimited-list #\} stream t))))

(flet ((rpar (str char)
         (declare (ignore str char))
         (get-macro-character #\) )))
  (defreadtable :on-lisp.17
    (:merge :standard)
    (:macro-char #\] #'rpar)
    (:macro-char #\} #'rpar)
    (:dispatch-macro-char #\# #\? #'|#?-reader|)
    (:dispatch-macro-char #\# #\[ #'|#[-reader|)
    (:dispatch-macro-char #\# #\{ #'|#{-reader|)))
