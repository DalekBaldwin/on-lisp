(in-package :on-lisp.24)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 24 - Prolog

;; p. 324

(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
       (let ,(mapcar (lambda (v)
                       `(,v (fullbind ',v binds)))
                     (vars-in query #'atom))
         ,@body
         (fail)))))

(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))

(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
                           (fullbind it b)
                           (gensym)))
        ((atom x) x)
        (t (cons (fullbind (car x) b)
                 (fullbind (cdr x) b)))))

;; ignoring same definition of varsym? as in chapter 18

;; p. 325

;; from the notes
#+nil
(defmacro check (expr)
  `(block nil
     (with-inference ,expr
       (return t))))

;; p. 326
#+nil
(=defun prove-query (expr binds)
  (case (car expr)
    (and (prove-and (cdr expr) binds))
    (or (prove-or (cdr expr) binds))
    (not (prove-not (cdr expr) binds))
    (t (prove-simple expr binds))))
#+nil
(=defun prove-and (clauses binds)
  (if (null clauses)
      (=values binds)
      (=bind (binds) (prove-query (car clauses) binds)
        (prove-and (cdr clauses) binds))))
#+nil
(=defun prove-or (clauses binds)
  (choose-bind c clauses
    (prove-query c binds)))
#+nil
(=defun prove-not (expr binds)
  (let ((save-path *paths*))
    (setq *paths* nil)
    (choose (=bind (b) (prove-query expr binds)
              (setq *paths* save-paths)
              (fail))
            (progn
              (setq *paths* save-paths)
              (=values binds)))))
#+nil
(=defun prove-simple (query binds)
  (choose-bind r *rlist*
    (implies r query binds)))

(=defuns
  (prove-query (expr binds)
               (case (car expr)
                 (and (prove-and (cdr expr) binds))
                 (or (prove-or (cdr expr) binds))
                 (not (prove-not (cdr expr) binds))
                 (t (prove-simple expr binds))))

  (prove-and (clauses binds)
             (if (null clauses)
                 (=values binds)
                 (=bind (binds) (prove-query (car clauses) binds)
                   (prove-and (cdr clauses) binds))))

  (prove-or (clauses binds)
            (choose-bind c clauses
              (prove-query c binds)))

  (prove-not (expr binds)
             (let ((save-path *paths*))
               (setq *paths* nil)
               (choose (=bind (b) (prove-query expr binds)
                         (setq *paths* save-paths)
                         (fail))
                       (progn
                         (setq *paths* save-paths)
                         (=values binds)))))

  (prove-simple (query binds)
                (choose-bind r *rlist*
                  (implies r query binds)))

  (implies (r query binds)
           (let ((r2 (change-vars r)))
             (aif2 (match query (cdr r2) binds)
                   (prove-query (car r2) it)
                   (fail)))))



;; p. 327

(defvar *rlist* nil)

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rlist* (rep_ (cons ',ant ',con))))))

#+nil
(=defun implies (r query binds)
  (let ((r2 (change-vars r)))
    (aif2 (match query (cdr r2) binds)
          (prove-query (car r2) it)
          (fail))))

(defun change-vars (r)
  (sublis (mapcar (lambda (v)
                    (cons v (symb '? (gensym))))
                  (vars-in r #'atom))
          r))



;; from the notes
#+nil
(=defun prove (query binds)
  (choose
   (choose-bind b2 (lookup (car query) (cdr query) binds)
     (=values b2))
   (choose-bind r *rules*
     (=funcall r query binds))))
