(in-package :on-lisp.23)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 23 - Parsing with ATNs

;; p. 310

;; name changed to avoid clash with chapter 6
(defmacro def-atn-node (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

;; I don't know how pg managed not to address this, but in this chapter there
;; are mutually referential sets of macros and functions -- using def-atn-node,
;; there is no way to define a set of nodes with a cycle because you'll have
;; to define a function that uses a macro that hasn't been defined yet. I guess
;; you can get away with this in interactive development as you keep reloading
;; definitions to close the reference loops, but it's no good for putting the
;; ideas from the book into testable packages. We can fix this by defining all
;; the macros first.
(defmacro def-atn-nodes (&body defns)
  `(=defuns
     ,@(loop for defn in defns
          collect `(,(car defn) (pos regs) (choose ,@(cdr defn))))))

(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
          (,next pos ,(compile-cmds cmds))))

(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos)
       (fail)
       (let ((* (nth pos *sent*)))
         (if (member ',cat (types *))
             (,next (1+ pos) ,(compile-cmds cmds))
             (fail)))))

(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

;; note on p. 208 about how this could be implemented with on-cdrs
#+nil
(defun compile-cmds (cmds)
  (on-cdrs `(,@it ,rec) 'regs cmds))

(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     (=values ,expr pos (cdr regs))))

(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     (if (cdr result) result (car result))))

(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
         (cdr ,regs)))

(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

(defmacro pushr (key val regs)
  `(set-register ',key
                 (cons ,val (cdr (assoc ',key (car ,regs))))
                 ,regs))

;; p. 314

(defparameter *types* nil)

#+nil
(defun types (w)
  (cdr (assoc w '((spot noun) (runs verb)))))

(defun types (w)
  (cdr (assoc w *types*)))

(defmacro with-parses (node sent &body body)
  (with-gensyms (pos regs)
    `(progn
       (setq *sent* ,sent)
       (setq *paths* nil)
       (=bind (parse ,pos ,regs) (,node 0 '(nil))
         (if (= ,pos (length *sent*))
             (progn ,@body (fail))
             (fail))))))
