;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 25 - Object-Oriented Lisp

;;##############################
(in-package :on-lisp.25.v1.test)
(in-readtable :on-lisp-test)

(in-root-suite)

(defsuite* test-chapter-25-v1)

(defun run-all-tests ()
  (format t "Testing chapter 25 object system version 1 |~%")
  (test-chapter-25-v1)
  (let ((results
         (run-tests :package :on-lisp.25.v1.test
                    :run-contexts #'with-summary-context)))))

(define-test test-blah ()
  (assert-expands
   (blarf)
   (blarf)))

;; p. 352
(deftest test-rget ()
  (let ((scoundrel (make-hash-table))
        (patriot (make-hash-table))
        (patriotic-scoundrel (make-hash-table)))
    (setf (gethash 'serves scoundrel) 'self
          (gethash 'serves patriot) 'country
          (gethash 'parents patriotic-scoundrel) (list scoundrel patriot))
    (is (eq (rget patriotic-scoundrel 'serves)
            'self))))


;;##############################
(in-package :on-lisp.25.v2.test)

(in-root-suite)

(defsuite* test-chapter-25-v2)

(defun run-all-tests ()
  (format t "Testing chapter 25 object system version 2 |~%")
  (test-chapter-25-v2)
  (let ((results
         (run-tests :package :on-lisp.25.v2.test
                    :run-contexts #'with-summary-context)))))

(define-test test-blah ()
  (assert-expands
   (blarf)
   (blarf)))

;; p. 354
(progn
  (setq patriot (obj))
  (setq scoundrel (obj))
  (setq patriotic-scoundrel (obj scoundrel patriot))
  (defprop serves))

(deftest test-defprop ()
  (setf (serves scoundrel) 'self)
  (setf (serves patriot) 'country)
  (is (eq (serves patriotic-scoundrel)
          'self)))

;;##############################
(in-package :on-lisp.25.v3.test)

(in-root-suite)

(defsuite* test-chapter-25-v3)

(defun run-all-tests ()
  (format t "Testing chapter 25 object system version 3 |~%")
  (test-chapter-25-v3)
  (let ((results
         (run-tests :package :on-lisp.25.v3.test
                    :run-contexts #'with-summary-context)))))

(define-test test-blah ()
  (assert-expands
   (blarf)
   (blarf)))

;; p. 357
(progn
  (setq rectangle (obj))
  (defprop height)
  (defprop width)
  (defmeth (area) rectangle (r)
    (* (height r) (width r))))

(deftest test-defmeth ()
  (is (=
       (let ((myrec (obj rectangle)))
         (setf (height myrec) 2
               (width myrec) 3)
         (area myrec))
       6)))

;; p. 359
(progn
  (setq filesystem (obj))
  (defmeth (backup :before) filesystem (fs)
    (format t "Remember to mount the tape.~%"))
  (defmeth (backup) filesystem (fs)
    (format t "Oops, deleted all your files.~%")
    'done)
  (defmeth (backup :after) filesystem (fs)
    (format t "Well, that was easy.~%")))

(deftest test-filesystem ()
  (is (equal
       (with-output-to-string (out-str)
         (let ((*standard-output* out-str))
           (backup (obj filesystem))))
       "Remember to mount the tape.
Oops, deleted all your files.
Well, that was easy.
")))

;;##############################
(in-package :on-lisp.25.v4.test)

(in-root-suite)

(defsuite* test-chapter-25-v4)

(defun run-all-tests ()
  (format t "Testing chapter 25 object system version 4 |~%")
  (test-chapter-25-v4)
  (let ((results
         (run-tests :package :on-lisp.25.v4.test
                    :run-contexts #'with-summary-context)))))

(define-test test-blah ()
  (assert-expands
   (blarf)
   (blarf)))

;; p. 360
#+nil
(progn
  (pop (parents patriotic-scoundrel))
  (serves patriotic-scoundrel))

;; p. 363
(progn
  (setq citrus (obj))
  (setq orange (obj citrus))
  ;; p. 364
  (setq my-orange (obj orange))
  (defmeth (props) citrus (c) '(round acidic))
  (defmeth (props) orange (o) '(orange sweet))
  (defmeth (props) my-orange (m) '(dented)))

(deftest test-orange ()
  (defcomb props (lambda (&rest args) (reduce #'union args)))
  (is (equal (props my-orange)
             #`(dented orange sweet round acidic)))
  (defcomb props :standard)
  (is (equal (props my-orange) #`(dented))))
