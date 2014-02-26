(in-package :on-lisp.18)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 18 - Destructuring

(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun destruc (pat seq &optional (atom? #'atom) (n 0))
    (if (null pat)
        nil
        (let ((rest (cond ((funcall atom? pat) pat)
                          ((eq (car pat) '&rest) (cadr pat))
                          ((eq (car pat) '&body) (cadr pat))
                          (t nil))))
          (if rest
              `((,rest (subseq ,seq ,n)))
              (let ((p (car pat))
                    (rec (destruc (cdr pat) seq atom? (1+ n))))
                (if (funcall atom? p)
                    (cons `(,p (elt ,seq ,n))
                          rec)
                    (let ((var (gensym)))
                      (cons (cons `(,var (elt ,seq ,n))
                                  (destruc p var atom?))
                            rec))))))))

  (defun dbind-ex (binds body)
    (if (null binds)
        `(progn ,@body)
        `(let ,(mapcar (lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                       binds)
           ,(dbind-ex (mapcan (lambda (b)
                                (if (consp (car b))
                                    (cdr b)))
                              binds)
                      body)))))

;; p. 234
(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1)
                   (col)) ;; missing in text
                  (mapcan
                   (lambda (pat)
                     (incf row)
                     (setq col -1)
                     (mapcar (lambda (p)
                               `(,p (aref ,gar
                                          ,row
                                          ,(incf col))))
                             pat))
                   pats))
         ,@body))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar (lambda (p)
                       `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))

;; p. 235
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar (lambda (f)
                       `(,f (,(symb name f) ,gs)))
                     fields)
         ,@body))))

;; p. 237
(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar (lambda (b)
                                   (if (consp (car b))
                                       (car b)
                                       b))
                                 binds)
         ,(wplac-ex (mapcan (lambda (b)
                              (if (consp (car b))
                                  (cdr b)))
                            binds)
                    body))))

;; p. 239
(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

;; p. 240

;; version 1
#+nil
(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar (lambda (v)
                         `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

;; p. 242

;; version 2
(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar (lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq)
                             (destruc pat gseq #'simple?))
                       then
                       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

;; p. 243
(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
         (cond ((gensym? pat)
                `(let ((,pat ,expr))
                   (if (and (typep ,pat 'sequence)
                            ,(length-test pat rest))
                       ,then
                       ,else)))
               ((eq pat '_) then)
               ((var? pat)
                (let ((ge (gensym)))
                  `(let ((,ge ,expr))
                     (if (or (gensym? ,pat) (equal ,pat ,ge))
                         (let ((,pat ,ge)) ,then)
                         ,else))))
               (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))
