(in-package :on-lisp.06)

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
