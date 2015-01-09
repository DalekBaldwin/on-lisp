;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 19 - A Query Compiler

;;#######################################
(in-package :on-lisp.19.interpreted.test)
(in-readtable :on-lisp-test)

(in-root-suite)

(defsuite* test-chapter-19-interpreted)

(defun run-all-tests ()
  (format t "Testing chapter 19 interpreted query system |~%")
  (test-chapter-19-interpreted)
  (let ((results
         (run-tests :package :on-lisp.19.interpreted.test
                    :run-contexts #'with-summary-context)))))

;; p. 252
(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

;; p. 253
(deftest hogarth ()
  (let ((answers))
    (with-answer (painter hogarth ?x ?y)
      (push (list ?x ?y) answers))
    (is (= (length answers) 1))
    (is (member #`(william english) answers :test #'equal))))

(deftest born-1697 ()
  (let ((answers))
    (with-answer (and (painter ?x _ _)
                       (dates ?x 1697 _))
      (push (list ?x) answers))
    (is (= (length answers) 2))
    (is (member #`(canale) answers :test #'equal))
    (is (member #`(hogarth) answers :test #'equal))))

(deftest died-1772-or-1792 ()
  (let ((answers))
    (with-answer (or (dates ?x ?y 1772)
                      (dates ?x ?y 1792))
      (push (list ?x ?y) answers))
    (is (= (length answers) 2))
    (is (member #`(hogarth 1697) answers :test #'equal))
    (is (member #`(reynolds 1723) answers :test #'equal))))

(deftest not-shared-birth-year ()
  (let ((answers))
    (with-answer (and (painter ?x _ english)
                       (dates ?x ?b _)
                       (not (and (painter ?x2 _ venetian)
                                 (dates ?x2 ?b _))))
      (push (list ?x) answers))
    (is (equal answers #`((reynolds))))))

;;#######################################
(in-package :on-lisp.19.compiled.test)

(in-root-suite)

(defsuite* test-chapter-19-compiled)

(defun run-all-tests ()
  (format t "Testing chapter 19 compiled query system |~%")
  (test-chapter-19-compiled)
  (let ((results
         (run-tests :package :on-lisp.19.compiled.test
                    :run-contexts #'with-summary-context)))))

(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

;; p. 257
(deftest hogarth ()
  (let ((answers))
    (with-answer (painter 'hogarth ?x ?y)
      (push (list ?x ?y) answers))
    (is (= (length answers) 1))
    (is (member #`(william english) answers :test #'equal))))

(deftest not-shared-birth-year ()
  (let ((answers))
    (with-answer (and (painter ?x _ 'english)
                      (dates ?x ?b _)
                      (not (and (painter ?x2 _ 'venetian)
                                (dates ?x2 ?b _))))
      (push (list ?x) answers))
    (is (equal answers #`((reynolds))))))

(deftest died-1770-to-1800 ()
  (let ((answers))
    (with-answer (and (painter ?x _ _)
                      (dates ?x _ ?d)
                      (lisp (< 1770 ?d 1800)))
      (push (list ?x ?d) answers))
    (is (= (length answers) 2))
    (is (member #`(reynolds 1792) answers :test #'equal))
    (is (member #`(hogarth 1772) answers :test #'equal))))
