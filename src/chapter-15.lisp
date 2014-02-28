(in-package :on-lisp.15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 15 - Macros Returning Functions

;; p. 202
(defmacro fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar (lambda (f)
                        `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                      (if fns
                          `(,(rbuild (car fns))
                             ,(rec (cdr fns)))
                          g)))
                (rec fns)))))

;; p. 205

;; remember, lrec does not yield tail-recursive solutions
(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec (lambda (it ,gfn)
             (symbol-macrolet ((rec (funcall ,gfn)))
               ,rec))
           ,base)))

#+nil
(defmacro alrec (rec &optional base)
  "cltl1 version"
  (let ((gfn (gensym)))
    `(lrec (lambda (it ,gfn)
             (labels ((rec () (funcall ,gfn)))
               ,rec))
           ,base)))

;; why do these functions take multiple lists when lrec and trec don't?
(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec (lambda () ,base)) ,@lsts))

;; p. 206
(defun our-copy-list (lst)
  (on-cdrs (cons it rec) nil lst))

(defun our-remove-duplicates (lst)
  (on-cdrs (adjoin it rec) nil lst))

(defun our-find-if (fn lst)
  (on-cdrs (if (funcall fn it) it rec) nil lst))

(defun our-some (fn lst)
  (on-cdrs (or (funcall fn it) rec) nil lst))

;; p. 207
(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

;; note how compile-cmds (p. 310) could be implemented with on-cdrs
#+nil
(defun compile-cmds (cmds)
  (on-cdrs `(,@it ,rec) 'regs cmds))

;; p. 210
(defmacro atrec (rec &optional (base 'it))
  "cltl2 version"
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec (lambda (it ,lfn ,rfn)
             (symbol-macrolet ((left (funcall ,lfn))
                               (right (funcall ,rfn)))
               ,rec))
           (lambda (it) ,base))))

#+nil
(defmacro atrec (rec &optional (base 'it))
  "cltl1 version"
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec (lambda (it ,lfn ,rfn)
             (labels ((left () (funcall ,lfn))
                      (right () (funcall ,rfn)))
               ,rec))
           (lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

(defun our-copy-tree (tree)
  (on-trees (cons left right) it tree))

(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))

;; new definition
#+nil
(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))

(defun rfind-if (fn tree)
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))

;; p. 211

;;(defconstant unforced (gensym))

;; changed as per:
;; http://coding.derkeiler.com/Archive/Lisp/comp.lang.lisp/2009-06/msg00968.html
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro unforced (load-time-value *unforced*)))
(defvar *unforced* (list :unforced))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             (lambda ()
               (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
          (delay-forced x))
      x))
