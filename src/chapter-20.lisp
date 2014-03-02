(in-package :on-lisp.20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 20 - Continuations

;; p. 267

;; http://www.paulgraham.com/onlisperrata.html:
;; p. 267. The global value of *cont* should be #'values instead of #'identity. Caught by Francois-Rene Rideau.

;; note from page 268:
;; Although *cont* has a global value, this will rarely be the one used:
;; *cont* will nearly always be a parameter, captured by =values and the
;; macros defined by =defun. Within the body of add1, for example, *cont*
;; is a parameter and not the global variable. This distinction is important
;; because these macros wouldn't work if *cont* were not a local variable.
;; That's why *cont* is given its initial value in a setq instead of a defvar:
;; the latter would also proclaim it to be special.

;;(setq *cont* #'identity)
(setq cont #'values)

(defmacro =lambda (parms &body body)
  `(lambda (cont ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f cont ,,@parms))
       (defun ,f (cont ,@parms) ,@body))))

;; needed for ATNs in chapter 23 and "prove-" clauses in chapter 24
(defmacro =defuns (&body defns)
  (let ((pairs
         (mapcar
          (lambda (defn)
            (cons defn
                  (intern (concatenate 'string "=" (symbol-name (car defn))))))
          defns)))
    `(progn
       ,@(loop for (defn . fname) in pairs
           collect `(defmacro ,(car defn) ,(cadr defn)
                      `(,',fname cont ,,@(cadr defn))))
       ,@(loop for (defn . fname) in pairs
             collect `(defun ,fname (cont ,@(cadr defn)) ,@(cddr defn))))))

(defmacro =bind (parms expr &body body)
  `(let ((cont (lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall cont ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn cont ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn cont ,@args))

;; from the notes -- maybe this is equivalent to Francois-Rene Rideau's fix from above?
#+nil
(setq *cont*
      #'(lambda (&rest args)
          (if (cdr args) args (car args))))

;;p. 271
(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)))))

(setq *saved* nil)

;; changed name from text -- SBCL will not let us
;; define a function named restart
(=defun call-restart ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))

;; changed definition order -- need call-restart macro
(=defun dft-node (tree)
  (cond ((null tree) (call-restart))
        ((atom tree) (=values tree))
        (t (push (lambda () (dft-node (cdr tree)))
                 *saved*)
           (dft-node (car tree)))))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) (=values nil))
          (t (princ node)
             (call-restart)))))
