(in-package :on-lisp)

;; a more readable version of the book with the missing figures re-added
;; can be found here: http://www.lurklurk.org/onlisp/onlisp.pdf

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 3 - Functional Programming

;; p. 29
(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

;; p. 30
(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;; p. 31
#+nil
(defun our-nreverse (lst)
  (if (null (cdr lst))
      lst
      (prog1 (nr2 lst)
        (setf (cdr lst) nil))))

#+nil
(defun nr2 (lst)
  (let ((c (cdr lst)))
    (prog1 (if (null (cdr c))
               c
               (nr2 c))
      (setf (cdr c) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 4 - Utility Functions

;; p. 45
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;; p. 47
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

#+nil
(defun filter (fn lst)
  (delete nil (mapcar fn lst)))

#+nil
(defun filter (fn lst)
  (mapcar (lambda (x)
            (let ((val (funcall fn x)))
              (if val (list val))))
          lst))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

#+nil
(defun group (source n)
  (if (endp source)
      nil
      (let ((rest (nthcdr n source)))
        (cons (if (consp rest) (subseq source 0 n) source)
              (group rest n)))))

;; p. 49
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

#+nil
(defun flatten (x)
  (mapcan (lambda (x)
            (if (atom x) (mklist x) (flatten x)))
          x))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

#+nil
(defun prune (test tree)
  (if (atom tree)
      tree
      (mapcar (lambda (x)
                (prune test x))
              (remove-if (lambda (y)
                           (and (atom y)
                                (funcall test y)))
                         tree))))

;; p. 50
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql)) 
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;; p. 52
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;; p. 54
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             (lambda (&rest args)
               (apply #'rmapcar fn args))
             args)))

;; p. 56
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
     (let ((in (apply #'prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~A~%" (funcall fn in))))))

;; p. 58
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list (lambda (c)
               (intern (make-string 1
                                    :initial-element c)))
       (symbol-name sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 5 - Returning Functions

;; p. 64
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs* fn)))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

;; p. 65
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
            (setf (gethash args cache)
                  (apply fn args)))))))

;; p. 66
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))

;; p. 67
(defun fif (if then &optional else)
  (lambda (x)
    (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        (lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        (lambda (x)
          (or (funcall fn x) (funcall chain x))))))

;; p. 69
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          (lambda ()
                            (self (cdr lst)))))))
    #'self))

;; p. 73
#+nil
(defun rfind-if (fn tree)
  (if (funcall fn tree)
      tree
      (if (atom tree)
          nil
          (or (rfind-if fn (car tree))
              (and (cdr tree) (rfind-if fn (cdr tree)))))))

;; p. 74
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

;; p. 75
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
                 base)
             (funcall rec tree
                      (lambda ()
                        (self (car tree)))
                      (lambda ()
                        (if (cdr tree)
                            (self (cdr tree))))))))
    #'self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 6 - Functions as Representation

;; p. 78
(defstruct node contents yes no)

#+nil
(defvar *nodes* (make-hash-table))

;; version 1
#+nil
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

;; p. 79
#+nil
(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n)))))
          (t (node-contents n)))))

;; version 2
#+nil
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
            (lambda ()
              (format t "~A~%>> " conts)
              (case (read)
                (yes (funcall (gethash yes *nodes*)))
                (t (funcall (gethash no *nodes*)))))
            (lambda () conts))))

;; p. 80
(defvar *nodes* nil)

;; version 3
(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n)))))
          (t (node-contents n)))))

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil (let ((conts (second node))
                  (yes (third node))
                  (no (fourth node)))
              (if yes
                  (let ((yes-fn (compile-net yes))
                        (no-fn (compile-net no)))
                    (lambda ()
                      (format t "~A~%>> " conts)
                      (funcall (if (eq (read) 'yes)
                                   yes-fn
                                   no-fn))))
                  (lambda () conts))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 8 - When to Use Macros

;; p. 114

;; version 1
#+nil
(defun move-objs (objs dx dy)
  (multiple-value-bind (x0 y0 x1 y1) (bound objs)
    (dolist (o objs)
      (incf (obj-x o) dx)
      (incf (obj-y) dy))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

#+nil
(defun scale-objs (objs factor)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (setf (obj-dx o) (* (obj-dx o) factor)
            (obj-dy o) (* (obj-dy o) factor)))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

;; p. 115

;; version 2
#+nil
(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
        (x1 (gensym)) (y1 (gensym)))
    `(let ((,gob ,objs))
       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
         (dolist (,var ,gob) ,@body)
         (multiple-value-bind (xa ya xb yb) (bounds ,gob)
           (redraw (min ,x0 xa) (min ,y0 ya)
                   (max ,x1 xb) (max ,y1 yb)))))))

#+nil
(defun move-objs (objs dx dy)
  (with-redraw (o objs)
    (incf (obj-x o) dx)
    (incf (obj-y o) dy)))

#+nil
(defun scale-objs (objs factor)
  (with-redraw (o objs)
    (setf (obj-dx o) (* (obj-dx o) factor)
          (obj-dy o) (* (obj-dy o) factor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 9 - Variable Capture

;; p. 127

;; version 1
#+nil
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

;; version 2
#+nil
(defmacro for ((var start stop) &body body)
  `(do ((b (lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

;; p. 129

;; version 3 -- correct
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 10 - Other Macro Pitfalls

;; p. 134

;; another incorrect version
#+nil
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

;; yet another
#+nil
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,gstop ,stop)
          (,var ,start (1+ ,var)))
         ((> ,var ,gstop))
       ,@body)))

;; p. 140
(defun ntha (n lst)
  (if (= n 0)
      (car lst)
      (ntha (- n 1) (cdr lst))))

;; incorrect
(defmacro nthb (n lst)
  `(if (= ,n 0)
       (car ,lst)
       (nthb (- ,n 1) (cdr ,lst))))

;; p. 141
(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst)
  (if (= n 0)
      (car lst)
      (nth-fn (- n 1) (cdr lst))))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
              (if (= n 0)
                  (car lst)
                  (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))

;; p. 142
(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               ,(or-expand (cdr args)))))))

(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               (orb ,@(cdr args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 11 - Classic Macros

;; p. 144
(defmacro our-let (binds &body body)
  `((lambda ,(mapcar (lambda (x)
                       (if (consp x) (car x) x))
                     binds)
      ,@body)
    ,@(mapcar (lambda (x)
                (if (consp x) (cadr x) nil))
              binds)))

;; p. 145
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
          (if ,(caar binds)
              (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

;; p. 146
(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar (lambda (v) (cons v (gensym)))
                      (remove-duplicates
                       (mapcar #'car
                               (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar (lambda (cl)
                         (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar (lambda (bindform)
            (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))))
          (cdr cl)))

;; p. 149

;; one version
#+nil
(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
            (progn
              (setq *db* ,db)
              (lock *db*)
              ,@body)
         (progn
           (release *db*)
           (setq *db* ,temp))))))

;; another
#+nil
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
    `(let ((,gbod (lambda () ,@body)))
       (declare (dynamic-extent ,gbod))
       (with-db-fn *db* ,db ,gbod))))

#+nil
(defun with-db-fn (old-db new-db body)
  (unwind-protect
       (progn
         (setq *db* new-db)
         (lock *db*)
         (funcall body))
    (progn
      (release *db*)
      (setq *db* old-db))))

;; p. 150
(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

;; p. 152
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar (lambda (a)
                        `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar (lambda (c)
                       `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar (lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

;; p. 154

;;;; repeated definitions of "while" and "for" omitted

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

;; p. 156

;; problem reported to pg but not checked -- test this!!!!!!!!!!
;; http://www.paulgraham.com/onlisperrata.html:
;; p. 156. In do-tuples/o the expression (1- (length parms)) should be
;; (- (length source) (length parms)).
;; Reported by Roland. (at netquant.com.br)
(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc (lambda ,parms ,@body)
                  ,@(map0-n (lambda (n)
                              `(nthcdr ,n ,src))
                            ;;(1- (length parms)) ;; error in text
                            (- (length source) (length parms)) ;; corrected
                            
                            ))))))

(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar (lambda (args)
                                  `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n (lambda (n)
                                       `(nth ,(1- n)
                                             ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n (lambda (m)
            (map1-n (lambda (n)
                      (let ((x (+ m n)))
                        (if (>= x len)
                            `(nth ,(- x len) ,src)
                            `(nth ,(1- x) ,rest))))
                    len))
          (- len 2)))

;; p. 159
(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
            ,label
            (if ,(car test)
                (return (progn ,@(cdr test))))
            ,@body
            ,@(mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
         (cons (list (if (atom (caar rebinds))
                         'setq
                         'multiple-value-setq)
                     (caar rebinds)
                     (third (car rebinds)))
               (mvdo-rebind-gen (cdr rebinds))))))

;; p. 161
(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar (lambda (p)
                         (mapcar (lambda (x)
                                   (declare (ignore x))
                                   (gensym))
                                 (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
                     ,@(mapcan (lambda (p s)
                                 (shuffle (mklist (car p))
                                          s))
                               pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
                                ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

;; p. 162
(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar (lambda (b)
                         (if (listp (car b))
                             (mapcar (lambda (x)
                                       (declare (ignore x))
                                       (gensym))
                                     (car b))
                             (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan (lambda (b var)
                            (list var (cadr b)))
                          binds
                          temps))
       (prog ,(mapcar (lambda (b var) (list b var))
                      (mappend #'mklist (mapcar #'car binds))
                      (mappend #'mklist temps))
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (mvpsetq ,@(mapcan (lambda (b)
                               (if (third b)
                                   (list (car b)
                                         (third b))))
                             binds))
          (go ,label)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 12 - Generalized Variables

;; p. 169
(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan (lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar (lambda (a) `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)

;; p. 170
(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

;; p. 173

;; get-setf-expansion replaces deprecated get-setf-method in the following
;; also, &environment args are added to the definitions

(defmacro _f (op place &rest args &environment env)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args &environment env)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place env)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args &environment env)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place env)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place &environment env)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place env)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

;; p. 176
(defmacro sortf (op &rest places)
  (let* ((meths (mapcar (lambda (p)
                          (multiple-value-list
                           (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan (lambda (m)
                              (append (first m)
                                      (third m)))
                            meths)
                    (mapcan (lambda (m)
                              (append (second m)
                                      (list (fifth m))))
                            meths))
       ,@(mapcon (lambda (rest)
                   (mapcar
                    (lambda (arg)
                      `(unless (,op ,(car rest) ,arg)
                         (rotatef ,(car rest) ,arg)))
                    (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

;; p. 178 -- is this the notion of "first class macro" Doug Hoyte talks about?

;; p. 179
#+nil
(defvar *cache* (make-hash-table))

#+nil
(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
        (values x y)
        (cdr (assoc key *world*)))))

#+nil
(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 13 - Computation at Compile-Time

;; p. 182
#+nil
(defun most-of (&rest args)
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar (lambda (a)
                       `(and ,a (> (incf ,hits) ,need)))
                     args)))))

;; p. 183
#+nil
(defun nthmost (n lst)
  (nth n (sort (copy-list lst) #'>)))

(defmacro nthmost (n lst)
  (if (and (integerp n) (< n 20))
      (with-gensyms (glst gi)
        (let ((syms (map0-n (lambda (x) (declare (ignore x)) (gensym)) n)))
          `(let ((,glst ,lst))
             (unless (< (length ,glst) ,(1+ n))
               ,@(gen-start glst syms)
               (dolist (,gi ,glst)
                 ,(nthmost-gen gi syms t))
               ,(car (last syms))))))
      `(nth ,n (sort (copy-list ,lst) #'>))))

(defun gen-start (glst syms)
  (reverse
   (maplist (lambda (syms)
              (let ((var (gensym)))
                `(let ((,var (pop ,glst)))
                   ,(nthmost-gen var (reverse syms)))))
            (reverse syms))))

(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
        (if (and (not long?) (null else))
            `(setq ,(car vars) ,var)
            `(if (> ,var ,(car vars))
                 (setq ,@(mapcan #'list
                                 (reverse vars)
                                 (cdr (reverse vars)))
                       ,(car vars) ,var)
                 ,else)))))

;; p. 187

(defconstant *segs* 20)
(defconstant *du* (/ 1.0 *segs*))
(defvar *pts* (make-array (list (1+ *segs*) 2)))

(defmacro genbez (x0 y0 x1 y1 x2 y2 x3 y3)
  (with-gensyms (gx0 gx1 gy0 gy1 gx3 gy3)
    `(let ((,gx0 ,x0) (,gy0 ,y0)
           (,gx1 ,x1) (,gy1 ,y1)
           (,gx3 ,x3) (,gy3 ,y3))
       (let ((cx (* (- ,gx1 ,gx0) 3))
             (cy (* (- ,gy1 ,gy0) 3))
             (px (* (- ,x2 ,gx1) 3))
             (py (* (- ,y2 ,gy1) 3)))
         (let ((bx (- px cx))
               (by (- py cy))
               (ax (- ,gx3 px ,gx0))
               (ay (- ,gy3 py ,gy0)))
           ,@(map1-n (lambda (n)
                       (let* ((u (* n *du*))
                              (u^2 (* u u))
                              (u^3 (expt u 3)))
                         `(setf (aref *pts* ,n 0)
                                (+ (* ax ,u^3)
                                   (* bx ,u^2)
                                   (* cx ,u)
                                   ,gx0)
                                (aref *pts* ,n 1)
                                (+ (* ay ,u^3)
                                   (* by ,u^2)
                                   (* cy ,u)
                                   ,gy0))))
                     (1- *segs*))
           (setf (aref *pts* *segs* 0) ,gx3
                 (aref *pts* *segs* 1) ,gy3))))))

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
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

;; p. 193
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
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

;; p. 199
(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t))))) ;; should this be eq instead?

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
                ,@body))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 16 - Macro-Defining Macros

;; p. 213
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

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

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
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))

(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 17 - Read-Macros

;; p. 225 - note how abbreviation for quote cannot be defined as normal macro
;; keep this in mind when using nested macrolet with macroexpand...

;; p. 226
(set-dispatch-macro-character #\# #\?
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    `(lambda (&rest ,(gensym))
       ,(read stream t nil t))))

;; p. 227
(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (let ((accum nil)
          (pair (read-delimited-list #\] stream t)))
      (do ((i (ceiling (car pair)) (1+ i)))
          ((> i (floor (cadr pair)))
           (list 'quote (nreverse accum)))
        (push i accum)))))

;; p. 228
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right (lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
      (lambda (stream char1 char2)
        (declare (ignore char1 char2))
        (apply fn
               (read-delimited-list right stream t))))))

;; p. 229
(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 19 - A Query Compiler

;; p. 248
(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
          ',args))

;; p. 251

;; interpreted version
(defmacro with-answer% (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar (lambda (v)
                       `(,v (binding ',v ,binds)))
                     (vars-in query #'atom))
         ,@body))))

(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t (lookup (car expr) (cdr expr) binds))))

(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan (lambda (b)
                (interpret-query (car clauses) b))
              (interpret-and (cdr clauses) binds))))

(defun interpret-or (clauses binds)
  (mapcan (lambda (c)
            (interpret-query c binds))
          clauses))

(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
      nil
      (list binds)))

(defun lookup (pred args &optional binds)
  (mapcan (lambda (x)
            (aif2 (match x args binds) (list it)))
          (db-query pred)))

;; p. 255

;; compiled version
(defmacro with-answer (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
     ,(compile-query query `(progn ,@body))))

(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cadr q) body))
    (lisp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defun compile-simple (q body)
  (let ((fact (gensym)))
    `(dolist (,fact (db-query ',(car q)))
       (pat-match ,(cdr q) ,fact ,body nil))))

(defun compile-and (clauses body)
  (if (null clauses)
      body
      (compile-query (car clauses)
                     (compile-and (cdr clauses) body))))

(defun compile-or (clauses body)
  (if (null clauses)
      nil
      (let ((gbod (gensym))
            (vars (vars-in body #'simple?)))
        `(labels ((,gbod ,vars ,body))
           ,@(mapcar #'(lambda (cl)
                         (compile-query cl `(,gbod ,@vars)))
                     clauses)))))

(defun compile-not (q body)
  (let ((tag (gensym)))
    `(if (block ,tag
           ,(compile-query q `(return-from ,tag nil))
           t)
         ,body)))

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

(defmacro =bind (parms expr &body body)
  `(let ((cont (lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall cont ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn cont ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn cont ,@args))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 21 - Multiple Processes

;; p. 277
(defstruct proc pri state wait)

(proclaim '(special *procs* *proc*))

;; compare to declaration of *unforced*
(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state (lambda (x)
                      (declare (ignore x))
                      (format t "~%>> ")
                      (princ (eval (read)))
                      (pick-process))))

(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc
            :state (lambda (,(gensym))
                     ,expr
                     (pick-process))
            :pri ,pri)
           *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
     (setq *procs* nil)
     ,@body
     (catch *halt* (loop (pick-process)))))

;; p. 280
(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
          *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
        (if (> pri max)
            (let ((val (or (not (proc-wait p))
                           (funcall (proc-wait p)))))
              (when val
                (setq proc1 p
                      max pri
                      val1 val))))))
    (values proc1 val1)))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
        (proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (parm test &body body)
  `(arbitrator (lambda () ,test)
               (lambda (,parm) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil (lambda (,(gensym)) ,@body)))

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 22 - Nondeterminism

;; p. 295
(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
      `(progn
         ,@(mapcar (lambda (c)
                     `(push (lambda () ,c) *paths*))
                   (reverse (cdr choices)))
         ,(car choices))
      '(fail)))

(defmacro choose-bind (var choices &body body)
  `(cb (lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
      (progn
        (if (cdr choices)
            (push (lambda () (cb fn (cdr choices)))
                  *paths*))
        (funcall fn (car choices)))
      (fail)))

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 23 - Parsing with ATNs

;; p. 310

;; name changed to avoid clash with chapter 6
(defmacro def-atn-node (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

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

;; p. 311


