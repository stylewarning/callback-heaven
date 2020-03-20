;;;; example.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith <quad@symbo1ics.com>

(in-package #:callback-heaven-examples)

;; First define your API group, and its API functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-api-group example))

(define-api-function (add example) :int ((a :int) (b :int))
  (ldb '#.(byte 32 0) (+ a b)))

(define-api-function (print-factorial example) :void ((n :int))
  (flet ((factorial (n)
           (let ((result 1))
             (loop :for i :from 1 :to n :do
               (setf result (* i result)))
             result)))
    (format t "Factorial ~A = ~A~%" n (factorial (abs n)))
    (values)))

(defvar *ctrans* (compute-c-space-translation (api-group 'example)))

(defun emit-library-api ()
  (emit-library-files *ctrans* "example.c" "example.h"))

;; If the above API changes, you need to re-emit the C-API and
;; re-compile the shared library. The included Makefile will handle both
;; of these steps for you, just run:
;;
;;   make clean; make
;;
;; For an example of loading and using the shared library, see the file
;; test.lisp.
