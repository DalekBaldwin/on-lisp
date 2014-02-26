(in-package :on-lisp.07)

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 7 - Macros

;; p. 86
;; with backquote
#+nil
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

;; without backquote
#+nil
(defmacro nif (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expr))
        (list 1 pos)
        (list 0 zero)
        (list -1 neg)))

;; p. 92
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;; p. 94
#+nil
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc (lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))

#+nil
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

;; p. 95
(defmacro our-expander (name) `(get ,name 'expander))

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             (lambda (,g)
               (block ,name
                 (destructuring-bind ,parms (cdr ,g)
                   ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))

;; p. 98
(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
        ,label
        (if ,test
            (return (progn ,@result)))
        ,@body
        (psetq ,@(make-stepforms bindforms))
        (go ,label))))

(defun make-initforms (bindforms)
  (mapcar (lambda (b)
            (if (consp b)
                (list (car b) (cadr b))
                (list b nil)))
          bindforms))

(defun make-stepforms (bindforms)
  (mapcan (lambda (b)
            (if (and (consp b) (third b))
                (list (car b) (third b))
                nil))
          bindforms))

;; p. 100
(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@(cdr args))))))

(defmacro our-andb (&rest args)
  (if (null args)
      t
      (labels ((expander (rest)
                 (if (cdr rest)
                     `(if ,(car rest)
                          ,(expander (cdr rest)))
                     (car rest))))
        (expander args))))
