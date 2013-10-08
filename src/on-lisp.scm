;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 20 - Continuations

;; p. 264
(define (dft tree)
    (cond ((null? tree) ())
          ((not (pair? tree)) (write tree))
          (else (dft (car tree))
                (dft (cdr tree)))))

(define *saved* ())

(define (dft-node tree)
    (cond ((null? tree) (restart))
          ((not (pair? tree)) tree)
          (else (call-with-current-continuation
                 (lambda (cc)
                   (set! *saved*
                         (cons (lambda ()
                                 (cc (dft-node (cdr tree))))
                               *saved*))
                   (dft-node (car tree)))))))

(define (restart)
    (if (null? *saved*)
        'done
        (let ((cont (car *saved*)))
          (set! *saved* (cdr *saved*))
          (cont))))

(define (dft2 tree)
    (set! *saved* ())
  (let ((node (dft-node tree)))
    (cond ((eq? node 'done) ())
          (else (write node)
                (restart)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chapter 22 - Nondeterminism

;; p. 290

;; deterministic
(define (descent n1 n2)
    (if (eq? n1 n2)
        (list n2)
        (let ((p (try-paths (kids n1) n2)))
          (if p (cons n1 p) #f))))

(define (try-paths ns n2)
    (if (null? ns)
        #f
        (or (descent (car ns) n2)
            (try-paths (cdr ns) n2))))

;; nondeterministic
(define (descent n1 n2)
    (cond ((eq? n1 n2) (list n2))
          ((null? (kids n1)) (fail))
          (else (cons n1 (descent (choose (kids n1)) n2)))))

;; p. 291
(define (two-numbers)
    (list (choose '(0 1 2 3 4 5))
          (choose '(0 1 2 3 4 5))))

(define (parlor-trick sum)
    (let ((nums (two-numbers)))
      (if (= (apply + nums) sum)
          `(the sum of ,@nums)
          (fail))))

;; p. 293
(define *paths* ())
(define failsym '@)

(define (choose choices)
    (if (null? choices)
        (fail)
        (call-with-current-continuation
         (lambda (cc)
           (set! *paths*
                 (cons (lambda ()
                         (cc (choose (cdr choices))))
                       *paths*))
           (car choices)))))

;;(define (choose choices)
;;  (if (null? choices)
;;      (fail)
;;      (call-with-current-continuation
;;       (lambda (cc)
;;         (push *paths*
;;               (lambda () (cc (choose (cdr choices)))))
;;         (car choices)))))

(define fail)

(call-with-current-continuation
 (lambda (cc)
   (set! fail
         (lambda ()
           (if (null? *paths*)
               (cc failsym)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))

;;(call-with-current-continuation
;; (lambda (cc)
;;   (define (fail)
;;     (if (null? *paths*)
;;         (cc failsym)
;;         ((pop *paths*))))))

;; p. 300

;; exhaustive search
(define (find-boxes)
  (set! *paths* ())
  (let ((city (choose '(la ny bos))))
    (newline)
    (let* ((store (choose '(1 2)))
           (box (choose '(1 2))))
      (let ((triple (list city store box)))
        (display triple)
        (if (coin? triple)
            (display 'c))
        (fail)))))

(define (coin? x)
  (member x '((la 1 2) (ny 1 1) (bos 2 2))))

;; p. 301
(define (mark) (set! *paths* (cons fail *paths*)))

;; http://www.paulgraham.com/onlisperrata.html:
;; p. 301 In Figure 22.9, equal? should be eq?. Caught by Francois-Rene Rideau.
(define (cut)
  (cond ((null? *paths*))
        ((eq? (car *paths*) fail)
         (set! *paths* (cdr *paths*)))
        (else
         (set! *paths* (cdr *paths*))
         (cut))))

;; pruned search
(define (find-boxes)
  (set! *paths* ())
  (let ((city (choose '(la ny bos))))
    (mark)
    (newline)
    (let* ((store (choose '(1 2)))
           (box (choose '(1 2))))
      (let ((triple (list city store box)))
        (display triple)
        (if (coin? triple)
            (begin (cut) (display 'c)))
        (fail)))))

;; p. 303

;; deterministic
(define (path node1 node2)
  (bf-path node2 (list (list node1))))

(define (bf-path dest queue)
  (if (null? queue)
      '@
      (let* ((path (car queue))
             (node (car path)))
        (if (eq? node dest)
            (cdr (reverse path))
            (bf-path dest
                     (append (cdr queue)
                             (map (lambda (n)
                                    (cons n path))
                                  (neighbors node))))))))

;; nondeterministic
(define (path node1 node2)
  (cond ((null? (neighbors node1)) (fail))
        ((memq node2 (neighbors node1)) (list node2))
        (else (let ((n (true-choose (neighbors node1))))
                (cons n (path n node2))))))

;; p. 304
(define *paths* ())
(define failsym '@)

(define (true-choose choices)
  (call-with-current-continuation
   (lambda (cc)
     (set! *paths* (append *paths*
                           (map (lambda (choice)
                                  (lambda () (cc choice)))
                                choices)))
     (fail))))

;;(define-syntax (true-choose choices)
;;  `(choose-fn ,choices ',(generate-symbol t)))

;;(define (choose-fn choices tag)
;;  (if (null? choices)
;;      (fail)
;;      (call-with-current-continuation
;;       (lambda (cc)
;;         (push *paths*
;;               (lambda () (cc (choose-fn (cdr choices)
;;                                         tag))))
;;         (if (mem equal? (car choices)
;;                  (table-entry *choice-pts* tag))
;;             (fail)
;;             (car (push (table-entry *choice-pts* tag)
;;                        (car choices))))))))

(define fail)

(call-with-current-continuation
 (lambda (cc)
   (set! fail
         (lambda ()
           (if (null? *paths*)
               (cc failsym)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))
