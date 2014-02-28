(in-package :on-lisp.05)

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

;; p. 68
#+nil
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

#+nil
(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))

;; p. 69

;; does not yield tail-recursive solutions
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

;; our-length
#+nil
(lrec (lambda (x f) (1+ (funcall f))) 0)

;; our-every for oddp
#+nil
(lrec (lambda (x f) (and (oddp x) (funcall f))) t)

;; p. 70

;; copy-list
#+nil
(lrec (lambda (x f) (cons x (funcall f))))

;; remove-duplicates
#+nil
(lrec (lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
#+nil
(lrec (lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
#+nil
(lrec (lambda (x f) (or (fn x) (funcall f))))


;; p. 72
#+nil
(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (if (cdr tree) (our-copy-tree (cdr tree))))))

#+nil
(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (or (if (cdr tree) (count-leaves (cdr tree)))
             1))))

#+nil
(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (flatten (cdr tree))))))

;; p. 73

;; will be redefined in chapter 15
#+nil
(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))


;; from the notes - variant that finds whole subtrees, not just atoms
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

;; our-copy-tree
#+nil
(ttrav #'cons)

;; count-leaves
#+nil
(ttrav (lambda (l r) (+ l (or r 1))) 1)

;; flatten
#+nil
(ttrav #'nconc #'mklist)

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

;; flatten
#+nil
(trec (lambda (o l r) (nconc (funcall l) (funcall r)))
      #'mklist)

;; rfind-if for oddp
#+nil
(trec (lambda (o l r) (or (funcall l) (funcall r)))
      (lambda (tree (and (oddp tree) tree))))
