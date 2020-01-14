;;;; callback-heaven.lisp
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

;; If your above API changes, you need to re-emit the C-API with the
;; following
;;
;;   (emit-library-files *ctrans* "example.c" "example.h")
;;
;; Then you will need to recompile the shared library
;;
;;   make clean; make

;; To load the shared library, make sure you are in the same directory
;; as the library and load it with:
;;
;; (cffi:define-foreign-library example
;;   (:darwin "libexample.dylib")
;;   (:unix   "libexample.so")
;;   (t (:default "libexample")))
;;
;; (cffi:use-foreign-library example)

;; CALLBACK-HEAVEN will also define a function (in the .c/.h files)
;; with the name set_function_index_group_<api-group-name>.  This
;; function will take in pointers to our defined lisp functions
;; (above) and set-up the trampoline structure in C.  We need to
;; inform CFFI of its existence:
;;
;; (cffi:defcfun ("set_function_index_group_example" %set-function-index-group-example)
;;     :void
;;   (functions (:pointer (:pointer :void))))
;;
;; (defun set-group-example-index ()
;;   (%set-function-index-group-example
;;    (c-space-translation-function-index *ctrans*)))
;;
;; (set-group-example-index)

;; In main.c we defined a function call_me_from_lisp, that will act as
;; our go-between.  Tell CFFI of its existence,
;;
;; (cffi:defcfun ("call_me_from_lisp" %call-me-from-lisp) :void)
;;
;; and invoke it with:
;;
;; CALLBACK-HEAVEN-EXAMPLES> (%call-me-from-lisp)
;; Factorial 5 = 120
;; NIL
