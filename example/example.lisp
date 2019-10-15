;;;; callback-heaven.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith <quad@symbo1ics.com>

(in-package #:callback-heaven-examples)

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

;; (emit-library-files *ctrans* "example.c" "example.h")
;;
;;; Make sure you're CD'd into the current directory.



(cffi:define-foreign-library example
  (:darwin "libexample.dylib")
  (:unix   "libexample.so")
  (t (:default "libexample")))

(cffi:use-foreign-library example)

(cffi:defcfun ("set_function_index_group_example" %set-function-index-group-example)
    :void
  (functions (:pointer (:pointer :void))))

(defun set-group-example-index ()
  (%set-function-index-group-example
   (callback-heaven::c-space-translation-function-index *ctrans*)))

;; (set-group-example-index)

(cffi:defcfun ("call_me_from_lisp" %call-me-from-lisp) :void)

;; CALLBACK-HEAVEN-EXAMPLES> (%call-me-from-lisp)
;; Factorial 5 = 120
;; NIL
