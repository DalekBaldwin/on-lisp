(in-package :on-lisp-test)

(defsuite* test-all)

(defun run-all-tests ()
  (test-all)
  (format t " |~%") ;; needed to avoid screwing up colors in SLIME REPL
  (run-tests :all :on-lisp-test))

(define-test test-blah
  ;; need to have at least one lisp-unit test to not barf in REPL
  (assert-expands
   (blarf)
   (blarf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 19 - A Query Compiler

;; p. 252
(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

;; p. 253
(deftest hogarth% ()
  (let ((answers))
    (with-answer% (painter hogarth ?x ?y)
      (push (list ?x ?y) answers))
    (is (= (length answers) 1))
    (is (member '(william english) answers :test #'equal))))

(deftest born-1697% ()
  (let ((answers))
    (with-answer% (and (painter ?x _ _)
                       (dates ?x 1697 _))
      (push (list ?x) answers))
    (is (= (length answers) 2))
    (is (member '(canale) answers :test #'equal))
    (is (member '(hogarth) answers :test #'equal))))

(deftest died-1772-or-1792% ()
  (let ((answers))
    (with-answer% (or (dates ?x ?y 1772)
                      (dates ?x ?y 1792))
      (push (list ?x ?y) answers))
    (is (= (length answers) 2))
    (is (member '(hogarth 1697) answers :test #'equal))
    (is (member '(reynolds 1723) answers :test #'equal))
    )
  )

(deftest not-shared-birth-year% ()
  (let ((answers))
    (with-answer% (and (painter ?x _ english)
                       (dates ?x ?b _)
                       (not (and (painter ?x2 _ venetian)
                                 (dates ?x2 ?b _))))
      (push (list ?x) answers))
    (is (equal answers '((reynolds))))))


;; p. 257
(deftest hogarth ()
  (let ((answers))
    (with-answer (painter 'hogarth ?x ?y)
      (push (list ?x ?y) answers))
    (is (= (length answers) 1))
    (is (member '(william english) answers :test #'equal))))

(deftest not-shared-birth-year ()
  (let ((answers))
    (with-answer (and (painter ?x _ 'english)
                      (dates ?x ?b _)
                      (not (and (painter ?x2 _ 'venetian)
                                (dates ?x2 ?b _))))
      (push (list ?x) answers)
      (is (equal answers '((reynolds)))))))

(deftest died-1770-to-1800 ()
  (let ((answers))
    (with-answer (and (painter ?x _ _)
                      (dates ?x _ ?d)
                      (lisp (< 1770 ?d 1800)))
      (push (list ?x ?d) answers)
      (is (= (length answers) 2))
      (is (member '(reynolds 1792) answers :test #'equal))
      (is (member '(hogarth 1772) answers :test #'equal)))))

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
  (is (equal (baz) '(hello there))))

;; p. 271
(deftest dft-test ()
  (setq t1 '(a (b (d h)) (c e (f i) g))
        t2 '(1 (2 (3 6 7) 4 5)))
  ;;(dft2 t1)
  )



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
  ;;(ballet)
  nil)

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
             '(the sum of 2 5))))
;; p. 299
(=defun descent (n1 n2)
  (cond ((eq n1 n2) (=values (list n2)))
        ((kids n1) (choose-bind n (kids n1)
                     (=bind (p) (descent n n2)
                       (=values (cons n1 p)))))
        (t (fail))))

(defun kids (n)
  (case n
    (a '(b c))
    (b '(d e))
    (c '(d f))
    (f '(g))))

(deftest descent-test ()
  (is (equal (descent 'a 'g)
             '(a c f g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 23 - An ATN Compiler
(def-atn-node s
    (down np s/subj
          (setr mood 'decl))
  (cat v v
       (setr mood 'imp)
       (setr subj '(np (pron you)))
       (setr aux nil)
       (setr v *)))

