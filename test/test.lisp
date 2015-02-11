;;;; callback-hell.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith <quad@symbo1ics.com>

(in-package #:callback-hell)

(define-api-group test)

(define-api-function (add test) :int ((a :int) (b :int))
  (ldb '#.(byte 32 0) (+ a b)))

(define-api-function (print-factorial test) :void ((n :int))
  (flet ((factorial (n)
           (let ((result 1))
             (loop :for i :from 1 :to n :do
               (setf result (* i result)))
             result)))
    (format t "Factorial ~A = ~A~%" n (factorial (abs n)))
    (values)))

(defvar *ctrans* (compute-c-space-translation (api-group 'test)))

;; (emit-library-files *ctrans* "test.c" "test.h")
;;
;;; Make sure you're CD'd into the current directory.



(cffi:define-foreign-library test
  (:darwin "libtest.dylib")
  (:unix   "libtest.so")
  (t (:default "libtest")))

(cffi:use-foreign-library test)

(cffi:defcfun ("set_function_index_group_test" %set-function-index-group-test)
    :void
  (functions (:pointer (:pointer :void))))

(defun set-group-test-index ()
  (%set-function-index-group-test
   (c-space-translation-function-index *ctrans*)))

;; (set-group-test-index)

(cffi:defcfun ("call_me_from_lisp" %call-me-from-lisp) :void)

;; CALLBACK-HELL> (%call-me-from-lisp)
;; Factorial 5 = 120
;; NIL
