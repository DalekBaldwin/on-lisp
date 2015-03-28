(in-package :on-lisp.14)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 14 - Anaphoric Macros

;; p. 191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))


;; problem noted by pg but no fix given
;; http://www.paulgraham.com/onlisperrata.html:
;; p. 191 (acond (3)) returns nil when it should return 3.
;; Same problem with acond2, p. 198. Caught by Terrence Ireland.
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
                 (declare (ignorable it)) ;; not in original code
                 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

;; p. 193

;; wrong
#+nil
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)))
        `(let ((it ,(car cl1)))
           (if it
               (progn ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         (declare (ignorable it)) ;; not in original code
                         ,(self (cdr args))))))
               args)))

;; p. 198
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro when2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))

;; problem noted by pg but no fix given
;; http://www.paulgraham.com/onlisperrata.html:
;; p. 191 (acond (3)) returns nil when it should return 3.
;; Same problem with acond2, p. 198. Caught by Terrence Ireland.
(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
                 (declare (ignorable it)) ;; not in original code
                 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

;; p. 199
(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t))))) ;; should this be eql instead?

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
                ,@body))))
