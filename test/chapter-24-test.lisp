;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 24 - Prolog

;;#######################################
(in-package :on-lisp.24.interpreted.test)
(in-readtable :on-lisp-test)

(in-root-suite)

(defsuite* test-chapter-24-interpreted)

(defun run-all-tests ()
  (format t "Testing chapter 24 interpreted Prolog |~%")
  (test-chapter-24-interpreted)
  (let ((results
         (run-tests :package :on-lisp.24.interpreted.test
                    :run-contexts #'with-summary-context)))))

;; p. 325
(deftest test-fullbind ()
  (let ((b #`((?x . (?y . ?z)) (?y . foo) (?z . nil))))
    (is (equal
         (fullbind '?x b)
         #`(foo)))))

;; p. 329
(deftest test-with-inference ()
  (setf on-lisp.24.interpreted::*rlist* nil)
  
  (<- (painter ?x) (hungry ?x)
       (smells-of ?x turpentine))
  (<- (hungry ?x) (or (gaunt ?x)
                       (eats-ravenously ?x)))
  (<- (gaunt raoul))
  (<- (smells-of raoul turpentine))
  (<- (painter rubens))

  (let ((results nil))
    (with-inference (painter ?x)
      (push ?x results))
    (is (equal
         (reverse results)
         #`(raoul rubens))))

  (<- (eats ?x ?f) (glutton ?x))
  (<- (glutton hubert))

  (let ((results nil))
    (with-inference (eats ?x spinach)
      (push ?x results))
    (is (equal
         (reverse results)
         #`(hubert))))
  ;; p. 330
  (let ((results nil))
    (with-inference (eats ?x ?y)
      (push (list ?x ?y) results))
    (is (every (lambda (result)
                 (and
                  (eq (car result) 'hubert)
                  (gensym? (cadr result))))
               (reverse results))))
  
  (<- (eats monster children))
  (<- (eats warhol candy))
  
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (eats ?x ?y)
               (format t "~A eats ~A.~%"
                       ?x
                       (if (gensym? ?y) 'everything ?y)))))))
    (is (equal
         printed
         "HUBERT eats EVERYTHING.
MONSTER eats CHILDREN.
WARHOL eats CANDY.
")))
  
  (<- (identical ?x ?x))
  
  (let ((result))
    (with-inference (identical a ?x)
      (push ?x result))
    (is (equal result #`(a))))
  
  ;; p. 331
  (<- (append nil ?xs ?xs))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
       (append ?xs ?ys ?zs))
  
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (append ?x (c d) (a b c d))
               (format t "Left: ~A~%" ?x))))))
    (is (equal
         printed
         "Left: (A B)
")))
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (append (a b) ?x (a b c d))
               (format t "Right: ~A~%" ?x))))))
    (is (equal
         printed
         "Right: (C D)
")))
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (append (a b) (c d) ?x)
               (format t "Whole: ~A~%" ?x))))))
    (is (equal
         printed
         "Whole: (A B C D)
")))
  ;; p. 332
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (append ?x ?y (a b c))
               (format t "Left: ~A  Right: ~A~%" ?x ?y))))))
    (is (equal
         printed
         "Left: NIL  Right: (A B C)
Left: (A)  Right: (B C)
Left: (A B)  Right: (C)
Left: (A B C)  Right: NIL
")))

  (<- (member ?x (?x . ?rest)))
  (<- (member ?x (_ . ?rest)) (member ?x ?rest))
  
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (member a (a b))
               (print t))))))
    (is (equal
         printed
         "
T ")))
  
  (<- (cara (a _)))
  
  (let ((printed
         (with-output-to-string (out-str)
           (let ((*standard-output* out-str))
             (with-inference (and (cara ?lst) (member b ?lst))
               (format t "~A" ?lst))))))
    (is (equal
         printed
         ;; fun fact: this broke paredit when I formatted it as in the previous
         ;; test and the open paren appeared as the first char on a new line
         "(A B)" 
         )))
  
  ;; p. 333
  (<- (all-elements ?x nil))
  (<- (all-elements ?x (?x . ?rest))
       (all-elements ?x ?rest))
  
  (let* ((result nil)
         (printed
          (with-output-to-string (out-str)
            (let ((*standard-output* out-str))
              (setf result
                    (block nil
                      (with-inference (all-elements a ?x)
                        (if (= (length ?x) 3)
                            (return ?x)
                            (princ ?x)))))))))
    (is (equal
         printed
         "NIL(A)(A A)" 
         ))
    (is (equal result #`(a a a))))
  )

;;####################################
(in-package :on-lisp.24.compiled.test)

(in-root-suite)

(defsuite* test-chapter-24-compiled)

(defun run-all-tests ()
  (format t "Testing chapter 24 compiled Prolog |~%")
  (test-chapter-24-compiled)
  (let ((results
         (run-tests :package :on-lisp.24.compiled.test
                    :run-contexts #'with-summary-context)))))

;; p. 337
(define-test test-with-inference-expand ()
  (assert-expands
   (WITH-GENSYMS (?X)
     (SETQ *PATHS* NIL)
     (=BIND (#:G1)
         (=BIND (#:G2)
             (ON-LISP.24.COMPILED::PROVE (LIST 'BIG ?X) NIL)
           (=BIND (#:G3)
               (ON-LISP.24.COMPILED::PROVE (LIST 'RED ?X) #:G2)
             (=VALUES #:G3)))
       (LET ((?X (ON-LISP.24.COMPILED::FULLBIND ?X #:G1)))
         (PRINT ?X))
       (FAIL)))
   (with-inference (and (big ?x) (red ?x))
     (print ?x))))

;;#########################################
(in-package :on-lisp.24.compiled-plus.test)

(in-root-suite)

(defsuite* test-chapter-24-compiled-plus)

(defun run-all-tests ()
  (format t "Testing chapter 24 fancy compiled Prolog |~%")
  (test-chapter-24-compiled-plus)
  (let ((results
         (run-tests :package :on-lisp.24.compiled-plus.test
                    :run-contexts #'with-summary-context)))))

;; p. 342
(deftest test-with-inference-compiled-plus ()
  (setf on-lisp.24.compiled-plus::*rules* nil)
  
  (<- (not-equal ?x ?x) (cut) (fail))
  (<- (not-equal ?x ?y))
  
  (is (null
       (let ((results nil))
         (with-inference (not-equal 'a 'a)
           (push t results))
         results)))
  (is (equal
       (let ((results nil))
         (with-inference (not-equal '(a a) '(b b))
           (push t results))
         results)
       #`(t)))
  ;; p. 343
  (<- (ordered (?x)))
  (<- (ordered (?x ?y . ?ys))
      (lisp (<= ?x ?y))
      (ordered (?y . ?ys)))

  (is (equal
       (let ((results nil))
         (with-inference (ordered '(1 2 3))
           (push t results))
         results)
       #`(t)))
  (is (null
       (let ((results nil))
         (with-inference (ordered '(1 3 2))
           (push t results))
         results)))
  
  (<- (factorial 0 1))
  (<- (factorial ?n ?f)
      (lisp (> ?n 0))
      (on-lisp.24.compiled-plus::is ?n1 (- ?n 1))
      (factorial ?n1 ?f1)
      (on-lisp.24.compiled-plus::is ?f (* ?n ?f1)))
  ;; p. 344
  (is (equal
       (let ((results nil))
         (with-inference (factorial 8 ?x)
           (push ?x results))
         results)
       #`(40320)))
  ;; p. 345
  (<- (append nil ?ys ?ys))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
      (append ?xs ?ys ?zs))
  
  (<- (quicksort (?x . ?xs) ?ys)
      (partition ?xs ?x ?littles ?bigs)
      (quicksort ?littles ?ls)
      (quicksort ?bigs ?bs)
      (append ?ls (?x . ?bs) ?ys))
  (<- (quicksort nil nil))
  
  (<- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
      (lisp (<= ?x ?y))
      (partition ?xs ?y ?ls ?bs))
  (<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
      (lisp (> ?x ?y))
      (partition ?xs ?y ?ls ?bs))
  (<- (partition nil ?y nil nil))

  (is (equal
       (let ((results nil))
         (with-inference (quicksort '(3 2 1) ?x)
           (push ?x results))
         results)
       #`((1 2 3))))
  
  (<- (echo)
      (on-lisp.24.compiled-plus::is ?x (read))
      (echo ?x))
  (<- (echo 'done)
      (cut))
  (<- (echo ?x)
      (lisp (prog1 t (format t "~A~%" ?x)))
      (on-lisp.24.compiled-plus::is ?y (read))
      (cut)
      (echo ?y))
  ;; p. 346
  (is (equal
       (with-output-to-string (out-str)
         (let ((*standard-output* out-str))
           (with-input-from-string (in-str "hi
ho
done
")
             (let ((*standard-input* in-str))
               (with-inference (echo))))))
       "HI
HO
")))
