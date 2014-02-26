(in-package :on-lisp.02)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 2 - Functions

;; p. 20
(defun make-dbms (db)
  (list
   (lambda (key)
     (cdr (assoc key db)))
   (lambda (key val)
     (push (cons key val) db) key)
   (lambda (key)
     (setf db (delete key db :key #'car)) key)))

;; p. 21
#+nil
(setq fact
      (lambda (f n)
        (if (= n 0)
            1
            (* n (funcall f f (- n 1))))))

#+nil
(defun recurser (fn)
  (lambda (&rest args)
    (apply fn fn args)))

;; p. 24
#+nil
(defun compall ()
  (do-symbols (s)
    (when (fboundp s)
      (unless (compiled-function-p (symbol-function s))
        (print s)
        (compile s)))))
