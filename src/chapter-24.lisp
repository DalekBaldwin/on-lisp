;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 24 - Prolog

;;##################################
(in-package :on-lisp.24.interpreted)

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

(defun change-vars (r)
  (sublis (mapcar (lambda (v)
                    (cons v (symb '? (gensym))))
                  (vars-in r #'atom))
          r))

;;###############################
(in-package :on-lisp.24.compiled)

(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))

;; p. 335

;; compiled version without cuts
(defmacro with-inference (query &rest body)
  (let ((vars (vars-in query #'simple?))
        (gb (gensym)))
    `(with-gensyms ,vars
       (setq *paths* nil)
       (=bind (,gb) ,(gen-query (rep_ query))
         (let ,(mapcar (lambda (v)
                         `(,v (fullbind ,v ,gb)))
                       vars)
           ,@body)
         (fail)))))

;; As in if-match or with-answer, pattern variables are initially bound to
;; gensyms to indicate that they haven't yet been assigned real values by
;; matching. Thus the function varsym?, which match and fullbind use to detect
;; variables, has to be changed to look for gensyms.

#+nil
(defun varsym? (x) ;; same definition as `gensym?`
  (and (symbolp x) (not (symbol-package x))))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((gensym? x) (values (cons (cons x y) binds) t))
   ((gensym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun fullbind (x b)
  (cond ((gensym? x) (aif2 (binding x b)
                           (fullbind it b)
                           (gensym)))
        ((atom x) x)
        (t (cons (fullbind (car x) b)
                 (fullbind (cdr x) b)))))

;; p. 336

(defun gen-query (expr &optional binds)
  (case (car expr)
    (and (gen-and (cdr expr) binds))
    (or (gen-or (cdr expr) binds))
    (not (gen-not (cadr expr) binds))
    (t `(prove (list ',(car expr)
                     ,@(mapcar #'form (cdr expr)))
               ,binds))))

(defun gen-and (clauses binds)
  (if (null clauses)
      `(=values ,binds)
      (let ((gb (gensym)))
        `(=bind (,gb) ,(gen-query (car clauses) binds)
           ,(gen-and (cdr clauses) gb)))))

(defun gen-or (clauses binds)
  `(choose
    ,@(mapcar #'(lambda (c) (gen-query c binds))
              clauses)))

(defun gen-not (clauses binds)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds)
                 (setq *paths* ,gpaths)
                 (fail))
               (progn
                 (setq *paths* ,gpaths)
                 (=values ,binds))))))

(=defun prove (query binds)
  (choose-bind r *rules* (=funcall r query binds)))

;; from the notes
#+nil
(=defun prove (query binds)
  (choose
   (choose-bind b2 (lookup (car query) (cdr query) binds)
     (=values b2))
   (choose-bind r *rules*
     (=funcall r query binds))))

(defun form (pat)
  (if (simple? pat)
      pat
      `(cons ,(form (car pat)) ,(form (cdr pat)))))

;; p. 338

(defvar *rules* nil)

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rules*
                     ,(rule-fn (rep_ ant) (rep_ con))))))

(defun rule-fn (ant con)
  (with-gensyms (val win fact binds)
    `(=lambda (,fact ,binds)
       (with-gensyms ,(vars-in (list ant con) #'simple?)
         (multiple-value-bind
               (,val ,win)
             (match ,fact
               (list ',(car con)
                     ,@(mapcar #'form (cdr con)))
               ,binds)
           (if ,win
               ,(gen-query ant val)
               (fail)))))))

;;####################################
(in-package :on-lisp.24.compiled-plus)

(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))

(defun form (pat)
  (if (simple? pat)
      pat
      `(cons ,(form (car pat)) ,(form (cdr pat)))))

(defun fullbind (x b)
  (cond ((gensym? x) (aif2 (binding x b)
                           (fullbind it b)
                           (gensym)))
        ((atom x) x)
        (t (cons (fullbind (car x) b)
                 (fullbind (cdr x) b)))))

(defvar *rules* nil)

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((gensym? x) (values (cons (cons x y) binds) t))
   ((gensym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rules*
                     ,(rule-fn (rep_ ant) (rep_ con))))))

;; p. 340

(defun rule-fn (ant con)
  (with-gensyms (val win fact binds paths)
    `(=lambda (,fact ,binds ,paths)
       (with-gensyms ,(vars-in (list ant con) #'simple?)
         (multiple-value-bind
               (,val ,win)
             (match ,fact
               (list ',(car con)
                     ,@(mapcar #'form (cdr con)))
               ,binds)
           (if ,win
               ,(gen-query ant val paths)
               (fail)))))))

(defmacro with-inference (query &rest body)
  (let ((vars (vars-in query #'simple?))
        (gb (gensym)))
    `(with-gensyms ,vars
       (setq *paths* nil)
       (=bind (,gb) ,(gen-query (rep_ query) nil '*paths*)
         (let ,(mapcar (lambda (v)
                         `(,v (fullbind ,v ,gb)))
                       vars)
           ,@body)
         (fail)))))

(defun gen-query (expr &optional binds paths)
  (case (car expr)
    (and (gen-and (cdr expr) binds paths))
    (or (gen-or (cdr expr) binds paths))
    (not (gen-not (cadr expr) binds paths))
    (lisp (gen-lisp (cadr expr) binds))
    (is (gen-is (cadr expr) (third expr) binds))
    (cut `(progn (setq *paths* ,paths)
                 (=values,binds)))
    (t `(prove (list ',(car expr)
                     ,@(mapcar #'form (cdr expr)))
               ,binds *paths*))))

(=defun prove (query binds paths)
  (choose-bind r *rules*
    (=funcall r query binds paths)))

;; p. 341

(defun gen-and (clauses binds paths)
  (if (null clauses)
      `(=values ,binds)
      (let ((gb (gensym)))
        `(=bind (,gb) ,(gen-query (car clauses) binds paths)
           ,(gen-and (cdr clauses) gb paths)))))

(defun gen-or (clauses binds paths)
  `(choose
    ,@(mapcar #'(lambda (c) (gen-query c binds paths))
              clauses)))

(defun gen-not (clauses binds paths)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds paths)
                 (setq *paths* ,gpaths)
                 (fail))
               (progn
                 (setq *paths* ,gpaths)
                 (=values ,binds))))))

(defmacro with-binds (binds expr)
  `(let ,(mapcar #'(lambda (v) `(,v (fullbind ,v ,binds)))
                 (vars-in expr))
     ,expr))

(defun gen-lisp (expr binds)
  `(if (with-binds ,binds ,expr)
       (=values ,binds)
       (fail)))

(defun gen-is (expr1 expr2 binds)
  `(aif2 (match ,expr1 (with-binds ,binds ,expr2) ,binds)
         (=values it)
         (fail)))
