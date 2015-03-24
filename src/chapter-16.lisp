(in-package :on-lisp.16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 16 - Macro-Defining Macros

;; p. 213

;; from the notes
#+nil
(let ((syms nil))
  (do-symbols (s)
    (push s syms))
  (sort syms
        (lambda (x y)
          (> (length (symbol-name x))
             (length (symbol-name y))))))

;; p. 214
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar (lambda (pair)
                 `(abbrev ,@pair))
               (group names 2))))

;; p. 216
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar (lambda (p) `(propmacro ,p))
               props)))

;; p. 217

;; from the notes
#+nil
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',propname)))

;; p. 219
(defmacro a+ (&rest args)
  (a+expand args nil))

(defun a+expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(a+expand (cdr args)
                      (append syms (list sym)))))
      `(+ ,@syms)))

(defmacro alist (&rest args)
  (alist-expand args nil))

(defun alist-expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(alist-expand (cdr args)
                          (append syms (list sym)))))
      `(list ,@syms)))

;; p. 220

;; version 1
#+nil
(defmacro defanaph (name &optional calls)
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))

(defun anaphex (args expr)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex (cdr args)
                     (append expr (list sym)))))
      expr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pop-symbol (sym)
    (intern (subseq (symbol-name sym) 1))))

;; p. 221

;; alternate versions
#+nil
(defanaph a+)

#+nil
(defanaph alist)

#+nil
(defmacro a+ (&rest args)
  (anaphex args '(+)))

;; p. 222

#+nil
(defmacro aif (&rest args)
  (anaphex2 'if args))

;; This sounds weird and I haven't really bothered to check this out, but hey,
;; this is what pg says:
;; http://www.paulgraham.com/onlisperrata.html:
;; p. 222. In the definition of asetf, 'setf should be '(lambda (x y) y).
;; Caught by Francois-Rene Rideau.

#+nil
(defmacro asetf (&rest args)
  (anaphex3 '(lambda (x y) y) args))

;; p. 223

;; version 2

;; http://www.paulgraham.com/onlisperrata.html:
;; p. 223. The &optional in the definition of defanaph is unnecessary.
;; Caught by Francois-Rene Rideau.
(defmacro defanaph (name &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           (declare (ignorable it))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))

(defun anaphex2 (op args)
  `(let ((it ,(car args)))
     (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args)))
       ,(car args)))

;; alternate version
#+nil
(defanaph aif :rule :first)

(defanaph asetf :rule :place)

#+nil
(defmacro incf (place &optional (val 1))
  `(asetf ,place ))

#+nil
(defmacro pull (obj place &rest args)
  `(asetf ,place (delete ,obj it ,@args)))
