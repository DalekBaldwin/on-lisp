(in-package :on-lisp-test)
(in-readtable :on-lisp-test)

(in-root-suite)

(defsuite* test-all)

(defun run-all-tests ()
  (format t "Testing chapters 2-18, 20-23 |~%") ;; "|" needed to avoid screwing up colors in SLIME REPL
  (test-all)
  (let ((results
         (run-tests :all :on-lisp-test)))
    (print-errors results)
    (print-failures results))
  (on-lisp.19.interpreted.test::run-all-tests)
  (on-lisp.19.compiled.test::run-all-tests)
  (on-lisp.24.interpreted.test::run-all-tests)
  (on-lisp.24.compiled.test::run-all-tests)
  (on-lisp.24.compiled-plus.test::run-all-tests)
  (on-lisp.25.v1.test::run-all-tests)
  (on-lisp.25.v2.test::run-all-tests)
  (on-lisp.25.v3.test::run-all-tests)
  (on-lisp.25.v4.test::run-all-tests))

(define-test test-blah
  ;; need to have at least one lisp-unit test to not barf in REPL
  (assert-expands
   (blarf)
   (blarf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 4 - Utility Functions

;; p. 47
(deftest test-filter ()
  (is (equal (filter (lambda (x) (if (numberp x) (1+ x)))
                     #`(a 1 2 b 3 c d 4))
             #`(2 3 4 5))))

;; p. 48
(deftest test-group ()
  (is (equal (group #`(a b c d e f g) 2)
             #`((a b) (c d) (e f) (g)))))

(deftest test-flatten ()
  (is (equal (flatten #`(a (b c) ((d e) f)))
             #`(a b c d e f))))

(deftest test-prune ()
  (is (equal (prune #'evenp #`(1 2 (3 (4 5) 6) 7 8 (9)))
             #`(1 (3 (5)) 7 (9)))))

;; p. 49
(deftest test-before ()
  (is (equal (before 'b 'd #`(a b c d))
             #`(b c d)))
  ;; p. 50
  (is (equal (before 'a 'b #`(a))
             #`(a))))

;; p. 51
(deftest test-after ()
  (is (equal (after 'a 'b #`(b a d))
             #`(a d)))
  (is (null (after 'a 'b #`(a))))
  (is (null (after 'b 'a #`(a)))) ;; I think this is the test pg meant to write
  )

(deftest test-duplicate ()
  (is (equal (duplicate 'a #`(a b c a d))
             #`(a d))))

(deftest test-split-if ()
  (multiple-value-bind (left right)
      (split-if (lambda (x) (> x 4))
                #`(1 2 3 4 5 6 7 8 9 10))
    (is (equal left #`(1 2 3 4)))
    (is (equal right #`(5 6 7 8 9 10)))))

(deftest test-most ()
  (is (equal (most #'length #`((a b) (a b c) (a) (e f g)))
             #`(a b c))))

;; p. 52
(deftest test-best ()
  (is (eql (best #'> #`(1 2 3 4 5)) 5)))

;; p. 53
(deftest test-mostn ()
  (is (equal (mostn #'length #`((a b) (a b c) (a) (e f g)))
             #`((a b c) (e f g)))))

(deftest test-map0-n ()
  (is (equal (map0-n #'1+ 5)
             (copy-list #`(1 2 3 4 5 6)))))

(deftest test-mapa-b ()
  (is (equal (mapa-b #'1+ -2 0 0.5)
             #`(-1 -0.5 0.0 0.5 1.0))))

;; p. 55
(deftest test-rmapcar ()
  (is (equal (rmapcar #'+ #`(1 (2 (3) 4)) #`(10 (20 (30) 40)))
             #`(11 (22 (33) 44)))))

;; p. 58
(deftest test-symb ()
  (let ((s (symb #`(a b))))
    (is (eq s '|(A B)|))
    (is (eq s '\(A\ B\)))))

;; p. 59
(deftest test-explode ()
  (is (equal (explode 'bomb)
             #`(b o m b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 11 - Classic Macros

;; p. 144
(deftest test-when-bind* ()
  (is (=
       (when-bind* ((x (find-if #'consp #`(a (1 2) b)))
                    (y (find-if #'oddp x)))
         (+ y 10))
       11)))

;; p. 147
(deftest test-condlet ()
  (is (equal
       (condlet (((= 1 2) (x 'a) (y 'b))
                 ((= 1 1) (y 'c) (x 'd))
                 (t (x 'e) (z 'f)))
         (list x y z))
       #`(d c nil))))

;; p. 151
(deftest test-nif ()
  (is (equal
       (mapcar #'(lambda (x)
                   (nif x 'p 'z 'n))
               #`(0 1 -1))
       #`(z p n))))

;; p. 163
(define-test test-mvdo
  (assert-expands
   (LET (#:G2 #:G3 #:G4)
     (MVPSETQ #:G2 1 (#:G3 #:G4) (VALUES 0 0))
     (PROG ((X #:G2) (Y #:G3) (Z #:G4))
        #:G1
        (IF (> X 5)
            (RETURN (PROGN (LIST X Y Z))))
        (PRINC (LIST X Y Z))
        (MVPSETQ X (1+ X) (Y Z) (VALUES Z X))
        (GO #:G1)))
   (mvdo ((x 1 (1+ x))
          ((y z) (values 0 0) (values z x)))
       ((> x 5) (list x y z)) (princ (list x y z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 12 - Generalized Variables

;; p. 168
(deftest test-toggle ()
  (is (equal
       (let ((lst #`(t nil t)) (i -1))
         (toggle (nth (incf i) lst))
         lst)
       #`(nil nil t))))

;; p. 174
(deftest test-pull ()
  (let ((x (copy-tree #`(1 2 (a b) 3))))
    (is (equal (pull 2 x)
               #`(1 (a b) 3)))
    (is (equal (pull #`(a b) x :test #'equal)
               #`(1 3)))
    (is (equal x
               #`(1 3)))))

;; p. 175
(deftest test-pull-if ()
  (let ((lst #`(1 2 3 4 5 6)))
    (pull-if #'oddp lst)
    (is (equal lst
               #`(2 4 6)))))

(deftest test-popn ()
  (let ((x #`(a b c d e f)))
    (is (equal (popn 3 x)
               #`(a b c)))
    (is (equal x
               #`(d e f)))))

(deftest test-sortf ()
  (let ((x 1)
        (y 2)
        (z 3))
    ;; In the text, this returns 3 but by looking at the expansion it should
    ;; clearly return 1. I'm guessing pg produced the output with an earlier
    ;; draft version of the macro.
    (is (equal (sortf > x y z)
               1)) 
    (is (equal (list x y z)
               #`(3 2 1)))))

;; p. 178
(deftest test-_f ()
  (let ((x 2))
    (_f nif x 'p 'z 'n)
    (is (eq x 'p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 13 - Computation at Compile-Time

;;p. 182
(define-test test-most-of
  (assert-expands
   (LET ((#:G1 0))
     (OR (AND (A) (> (INCF #:G1) 1)) (AND (B) (> (INCF #:G1) 1))
         (AND (C) (> (INCF #:G1) 1))))
   (most-of (a) (b) (c))))

;; p. 185
(define-test test-nthmost
  (assert-expands
   (LET ((#:G1 NUMS))
     (UNLESS (< (LENGTH #:G1) 3)
       (LET ((#:G8 (POP #:G1)))
         (SETQ #:G3 #:G8))
       (LET ((#:G7 (POP #:G1)))
         (IF (> #:G7 #:G3)
             (SETQ #:G4 #:G3
                   #:G3 #:G7)
             (SETQ #:G4 #:G7)))
       (LET ((#:G6 (POP #:G1)))
         (IF (> #:G6 #:G3)
             (SETQ #:G5 #:G4
                   #:G4 #:G3
                   #:G3 #:G6)
             (IF (> #:G6 #:G4)
                 (SETQ #:G5 #:G4
                       #:G4 #:G6)
                 (SETQ #:G5 #:G6))))
       (DOLIST (#:G2 #:G1)
         (IF (> #:G2 #:G3)
             (SETQ #:G5 #:G4
                   #:G4 #:G3
                   #:G3 #:G2)
             (IF (> #:G2 #:G4)
                 (SETQ #:G5 #:G4
                       #:G4 #:G2)
                 (IF (> #:G2 #:G5)
                     (SETQ #:G5 #:G2)
                     NIL))))
       #:G5))
   (nthmost 2 nums)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 15 - Macros Returning Functions

;; p. 203
(deftest test-fn ()
  (is (equal (mapcar (fn (and integerp oddp)) #`(c 3 p 0))
             #`(nil t nil nil)))
  (is (equal (mapcar (fn (or integerp symbolp)) #`(c 3 p 0.2))
             #`(t t t nil)))
  (is (equal (map1-n (fn (if oddp 1+ identity)) 6)
             #`(2 2 4 4 6 6)))
  (is (equal (mapcar (fn (list 1- identity 1+))
                     #`(1 2 3))
             #`((0 1 2) (1 2 3) (2 3 4))))
  (is (equal (remove-if (fn (or (and integerp oddp)
                                (and consp cdr)))
                        #`(1 (a b) c (d) 2 3.4 (e f g)))
             #`(c (d) 2 3.4))))

;; p. 206
(deftest test-unions ()
  ;; ordering not predictable in general, but whatever
  (is (equal (unions #`(a b) #`(b c) #`(c d))
             #`(a d b c))))

;; p. 207
(deftest test-differences ()
  (is (equal (differences #`(a b c d e) #`(a f) #`(d))
             #`(b c e))))

(deftest test-maxmin ()
  (is (equal (multiple-value-bind
                   (max min) (maxmin #`(3 4 2 8 5 1 6 7))
               (list max min))
             #`(8 1))))

;; p. 211
(deftest test-delay-force ()
  (is (eq (force 'a) 'a))
  (let ((d (delay (1+ 2))))
    (is (= (force d) 3))
    (is (= (force d) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 16 - Macro-Defining Macros

;; p. 219
(defun mass-cost (menu-price)
    (a+ menu-price (* it .05) (* it 3)))

(deftest test-a+ ()
  (is (= (mass-cost 7.95)
         9.54)))

(deftest test-alist ()
  (is (equal (alist 1 (+ 2 it) (+ 2 it))
             #`(1 3 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 17 - Read-Macros

;; p. 226
(deftest test-#? ()
  (is (equal (mapcar #?2 #`(a b c))
             #`(2 2 2)))
  ;; p. 227
  (is (eq (funcall #?'a) 'a))
  (is (eq (funcall #?#'oddp) (symbol-function 'oddp))))

(deftest test-sharp-brackets ()
  (is (equal #[2 7]
             #`(2 3 4 5 6 7))))

;; p. 229
(deftest test-sharp-braces ()
  (is (equal (funcall #{list 1+} 7)
             #`(8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 18 - Destructuring

;; p. 231
(deftest test-dbind ()
  (is (equal (dbind (a b c) #(1 2 3) (list a b c))
             #`(1 2 3)))
  (is (equal (dbind (a (b c) d) #`(1 #(2 3) 4) (list a b c d))
             #`(1 2 3 4)))
  (is (equal (dbind (a (b . c) &rest d) #`(1 "fribble" 2 3 4) (list a b c d))
             #`(1 #\f "ribble" (2 3 4)))))

;; p. 233
(deftest test-destruc ()
  (is (equal (destruc #`(a b c) 'seq #'atom)
             #`((a (elt seq 0)) (b (elt seq 1)) (c (elt seq 2))))))


(defmacro destruc-macro (pat seq)
  (destruc pat seq))

(define-test test-destruc-expand
  (assert-expands
   ((A (ELT SEQ 0))
    ((#:G1 (ELT SEQ 1))
     (B (ELT #:G1 0))
     (C (SUBSEQ #:G1 1)))
    (D (SUBSEQ SEQ 2)))
   (destruc-macro (a (b . c) &rest d) seq)))

(defmacro dbind-ex-macro (binds body)
  (dbind-ex binds body))

(define-test test-dbind-ex-expand
  (assert-expands
   (LET ((A (ELT SEQ 0))
         (#:G1 (ELT SEQ 1))
         (D (SUBSEQ SEQ 2)))
     (LET ((B (ELT #:G1 0))
           (C (SUBSEQ #:G1 1)))
       (PROGN BODY)))
   (dbind-ex-macro
    ((A (ELT SEQ 0))
     ((#:G1 (ELT SEQ 1))
      (B (ELT #:G1 0))
      (C (SUBSEQ #:G1 1)))
     (D (SUBSEQ SEQ 2)))
    (body))))

;; p. 235
(let ((ar (make-array #`(3 3))))
  (deftest test-with-matrix ()
    (for (r 0 2)
      (for (c 0 2)
        (setf (aref ar r c) (+ (* r 10) c))))
    (is (equal (with-matrix ((a b c)
                             (d e f)
                             (g h i)) ar
                 (list a b c d e f g h i))
               #`(0 1 2 10 11 12 20 21 22))))
  (deftest test-with-array ()
    (is (equal (with-array ((a 0 0) (d 1 1) (i 2 2)) ar
                 (list a d i))
               #`(0 11 22)))))

;; p. 236
(deftest test-with-struct ()
  (defstruct visitor name title firm)
  (let ((theo
         (make-visitor :name "Theodebert"
                       :title 'king
                       :firm 'franks)))
    (is (equal (with-struct (visitor- name firm title) theo
                 (list name firm title))
               #`("Theodebert" franks king)))))

(deftest test-with-places ()
  (is (equal (with-places (a b c) #(1 2 3)
               (list a b c))
             #`(1 2 3)))
  ;; p. 237
  (is (equal
       (let ((lst #`(1 (2 3) 4)))
         (with-places (a (b . c) d) lst
           (setf a 'uno)
           (setf c '(tre)))
         lst)
       #`(uno (2 tre) 4))))

;; p. 238

(deftest test-match ()
  (is (equal (multiple-value-bind (binds matched)
                 (match #`(p a b c a) #`(p ?x ?y c ?x))
               (list binds matched))
             #`(((?y . b) (?x . a))
                t)))
  ;; p. 239
  (is (equal (multiple-value-bind (binds matched)
                 (match #`(p ?x b ?y a) #`(p ?y b c a))
               (list binds matched))
             #`(((?y . c) (?x . ?y))
                t)))
  (is (equal (multiple-value-bind (binds matched)
                 (match #`(a b c) #`(a a a))
               (list binds matched))
             #`(nil nil)))
  (is (equal (multiple-value-bind (binds matched)
                 (match #`(p ?x) #`(p ?x))
               (list binds matched))
             #`(nil t)))
  ;; p. 240
  (is (equal (multiple-value-bind (binds matched)
                 (match #`(a ?x b) #`(_ 1 _))
               (list binds matched))
             #`(((?x . 1))
                t))))

;; p. 241
(deftest test-if-match ()
  (flet ((abab (seq)
           (if-match (?x ?y ?x ?y) seq
                     (values ?x ?y)
                     nil)))
    (is (equal (multiple-value-bind (x y)
                   (abab #`(hi ho hi ho))
                 (list x y))
               #`(hi ho)))
    ;; p. 244
    (is (=
         (let ((n 3))
           (if-match (?x n 'n '(a b)) '(1 3 n (a b))
                     ?x))
         1))
    ;; p. 245
    (is (equal (multiple-value-bind (a b)
                   (abab "abab")
                 (list a b))
               (list #\a #\b)))
    (is (equal (multiple-value-bind (a b)
                   (abab #(1 2 1 2))
                 (list a b))
               #`(1 2)))
    (is (equalp (multiple-value-bind (x y)
                    (if-match (?x (1 . ?y) . ?x) #`((a b) #(1 2 3) a b)
                              (values ?x ?y))
                  (list x y))
                #`((a b) #(2 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 20 - Continuations

;; p. 268
(=defun message ()
  (=values 'hello 'there))

;; p. 269
(=defun baz ()
  (=bind (m n) (message)
    (=values (list m n))))

(deftest cont-test ()
  (is (equal (baz) #`(hello there))))

;; p. 271
(deftest dft-test ()
  (let ((t1 #`(a (b (d h)) (c e (f i) g)))
        (t2 #`(1 (2 (3 6 7) 4 5))))
    (is (equal
         (with-output-to-string (str)
           (let ((*standard-output* str))
             (dft2 t1)))
         "ABDHCEFIG"))
    ;; p. 272
    (is (equal
         (let ((results nil))
           (=bind (node1) (dft-node t1)
             (if (eq node1 'done)
                 'done
                 (=bind (node2) (dft-node t2)
                   (push (list node1 node2) results)
                   (list node1 node2))))
           (loop for result = (call-restart)
              while (not (member 'on-lisp.20::done result)))
           (reverse (cdr results)))
         #`((A 1) (A 2) (A 3) (A 6) (A 7) (A 4) (A 5) (B 1) (B 2) (B 3) (B 6) (B 7)
            (B 4) (B 5) (D 1) (D 2) (D 3) (D 6) (D 7) (D 4) (D 5) (H 1) (H 2) (H 3)
            (H 6) (H 7) (H 4) (H 5) (C 1) (C 2) (C 3) (C 6) (C 7) (C 4) (C 5) (E 1)
            (E 2) (E 3) (E 6) (E 7) (E 4) (E 5) (F 1) (F 2) (F 3) (F 6) (F 7) (F 4)
            (F 5) (I 1) (I 2) (I 3) (I 6) (I 7) (I 4) (I 5) (G 1) (G 2) (G 3) (G 6)
            (G 7) (G 4) (G 5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 21 - Multiple Processes

;; p. 282
(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
  (format t "Approach ~A. " door)
  (claim 'knock door)
  (wait d (check 'open door)
    (format t "Enter ~A. " door)
    (unclaim 'knock door)
    (claim 'inside door)))

(=defun host (door)
  (wait k (check 'knock door)
    (format t "Open ~A. " door)
    (claim 'open door)
    (wait g (check 'inside door)
      (format t "Close ~A.~%" door)
      (unclaim 'open door))))

(program ballet ()
  (fork (visitor 'door1) 1)
  (fork (host 'door1) 1)
  (fork (visitor 'door2) 1)
  (fork (host 'door2) 1))

(deftest ballet-test ()
  (is (equal
       (let ((*bboard* nil))
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-input-from-string (in-str "(halt)")
               (let ((*standard-input* in-str))
                 (ballet))))))
       "Approach DOOR2. Open DOOR2. Enter DOOR2. Close DOOR2.
Approach DOOR1. Open DOOR1. Enter DOOR1. Close DOOR1.

>> ")))

;; p. 283
(=defun capture (city)
  (take city)
  (setpri 1)
  (yield
    (fortify city)))

(=defun plunder (city)
  (loot city)
  (ransom city))

(defun take (c) (format t "Liberating ~A.~%" c))
(defun fortify (c) (format t "Rebuilding ~A.~%" c))
(defun loot (c) (format t "Nationalizing ~A.~%" c))
(defun ransom (c) (format t "Refinancing ~A.~%" c))

(program barbarians ()
  (fork (capture 'rome) 100)
  (fork (plunder 'rome) 98))

(deftest barbarians-test ()
  (is (equal
       (with-output-to-string (out-str)
         (let ((*standard-output* out-str))
           (with-input-from-string (in-str "(halt)")
             (let ((*standard-input* in-str))
               (barbarians)))))
       "Liberating ROME.
Nationalizing ROME.
Refinancing ROME.
Rebuilding ROME.

>> ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 22 - Nondeterminism

;; p. 297
(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
        `(the sum of ,n1 ,n2)
        (fail))))

(deftest parlor-trick-test ()
  (is (equal (parlor-trick 7)
             #`(the sum of 2 5))))
;; p. 299
(=defun descent (n1 n2)
  (cond ((eq n1 n2) (=values (list n2)))
        ((kids n1) (choose-bind n (kids n1)
                     (=bind (p) (descent n n2)
                       (=values (cons n1 p)))))
        (t (fail))))

(defun kids (n)
  (case n
    (a #`(b c))
    (b #`(d e))
    (c #`(d f))
    (f #`(g))))

(deftest descent-test ()
  (is (equal (descent 'a 'g)
             #`(a c f g)))
  (is (equal (descent 'a 'd)
             #`(a b d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 23 - An ATN Compiler

;; p. 307

(def-atn-nodes
  (s1
   (cat noun s2
        (setr subj *)))

  (s2
   (cat verb s3
        (setr v *)))

  (s3
   (up `(sentence
         (subject ,(getr subj))
         (verb ,(getr v))))))

;; p. 313
(deftest test-compile-cmds ()
  (is (equal (compile-cmds '((setr a b) (setr c d)))
             #`(setr a b (setr c d on-lisp.23::regs))))
  (is (equal (compile-cmds '((setr a b) (progn (princ "ek!")) (setr c d)))
             #`(setr a b (progn (princ "ek!") (setr c d on-lisp.23::regs))))))

;; p. 314
(deftest test-with-parses ()
  (let* ((*types* #`((spot noun) (runs verb)))
         (parsed
          (with-output-to-string (out-str)
            (let ((*standard-output* out-str))
              (with-parses s1 '(spot runs)
                (format t "Parsing: ~A~%" parse))))))
    (is (equal
         parsed
         "Parsing: (SENTENCE (SUBJECT SPOT) (VERB RUNS))
"))))

;; p. 315
(defparameter *time-arrow-types*
  (loop for cases in
       #`(((do does did) (aux v))
          ((time times) (n v))
          ((fly flies) (n v))
          ((like) (v prep))
          ((liked likes) (v))
          ((a an the) (det))
          ((arrow arrows) (n))
          ((i you he she him her it) (pron)))
     appending (loop for word in (car cases)
                  collect (cons word (cadr cases)))))

;; p. 316
(def-atn-nodes
  (mods
   (cat n mods/n
        (setr mods *)))

  (mods/n
   (cat n mods/n
        (pushr mods *))
   (up `(n-group ,(getr mods)))))

(deftest test-time-arrow-with-parses ()
  (let* ((*types* *time-arrow-types*)
         (parsed
          (with-output-to-string (out-str)
            (let ((*standard-output* out-str))
              (with-parses mods '(time arrow)
                (format t "Parsing: ~A~%" parse))))))
    (is (equal
         parsed
         "Parsing: (N-GROUP (ARROW TIME))
"))))

;; p. 317
(def-atn-nodes
  (np
   (cat det np/det
        (setr det *))
   (jump np/det
         (setr det nil))
   (cat pron pron
        (setr n *)))

  (pron
   (up `(np (pronoun ,(getr n)))))

  (np/det
   (down mods np/mods
         (setr mods *))
   (jump np/mods
         (setr mods nil)))

  (np/mods
   (cat n np/n
        (setr n *)))

  (np/n
   (up `(np (det ,(getr det))
            (modifiers ,(getr mods))
            (noun ,(getr n))))
   (down pp np/pp
         (setr pp *)))

  (np/pp
   (up `(np (det ,(getr det))
            (modifiers ,(getr mods))
            (noun ,(getr n))
            ,(getr pp))))

  (pp
   (cat prep pp/prep
        (setr prep *)))

  (pp/prep
   (down np pp/np
         (setr op *)))

  (pp/np
   (up `(pp (prep ,(getr prep))
            (obj ,(getr op))))))

(deftest test-parse-np ()
  (let ((*types* *time-arrow-types*)
        (parses nil))
    (with-parses np '(it)
      (push parse parses))
    (is (equal parses #`((NP (PRONOUN IT)))))
    (setf parses nil)
    (with-parses np '(arrows)
      (push parse parses))
    (is (equal parses #`((NP (DET NIL) (MODIFIERS NIL) (NOUN ARROWS)))))
    (setf parses nil)
    (with-parses np '(a time fly like him)
      (push parse parses))
    (is (equal parses
               #`((NP (DET A) (MODIFIERS (N-GROUP TIME)) (NOUN FLY)
                      (PP (PREP LIKE) (OBJ (NP (PRONOUN HIM))))))))))

;; p. 319
(def-atn-nodes
  (s
   (down np s/subj
         (setr mood 'decl)
         (setr subj *))
   (cat v v
        (setr mood 'imp) 
        (setr subj '(np (pron you)))
        (setr aux nil)
        (setr v *)))

  (s/subj
   (cat v v
        (setr aux nil)
        (setr v *)))

  (v
   (up `(s (mood ,(getr mood))
           (subj ,(getr subj))
           (vcl (aux ,(getr aux))
                (v ,(getr v)))))
   (down np s/obj
         (setr obj *)))

  (s/obj
   (up `(s (mood ,(getr mood))
           (subj ,(getr subj))
           (vcl (aux ,(getr aux))
                (v ,(getr v)))
           (obj ,(getr obj))))))

(deftest test-parse-s ()
  (let ((*types* *time-arrow-types*)
        (parses nil))
    (with-parses s '(time flies like an arrow)
      (push parse parses))
    (is (equal parses
               #`((S (MOOD IMP) (SUBJ (NP (PRON YOU))) (VCL (AUX NIL) (V TIME))
                     (OBJ
                      (NP (DET NIL) (MODIFIERS NIL) (NOUN FLIES)
                          (PP (PREP LIKE)
                              (OBJ (NP (DET AN) (MODIFIERS NIL) (NOUN ARROW)))))))
                  (S (MOOD DECL)
                     (SUBJ (NP (DET NIL) (MODIFIERS (N-GROUP TIME)) (NOUN FLIES)))
                     (VCL (AUX NIL) (V LIKE))
                     (OBJ (NP (DET AN) (MODIFIERS NIL) (NOUN ARROW)))))))))
