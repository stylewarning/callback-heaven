;;;; test.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith <quad@symbo1ics.com>

(in-package #:callback-heaven-examples)

(assert (or (probe-file "libexample.dylib")
            (probe-file "libexample.so"))
        ()
        "This file should only be loaded *after* generating the libexample shared library. ~@
         You can run the code in this file by running \"make test\" in the examples directory.")

;; To load the shared library, make sure you are in the same directory
;; as the library and load it with:
(cffi:define-foreign-library example
  (:darwin "libexample.dylib")
  (:unix   "libexample.so")
  (t (:default "libexample")))

(cffi:use-foreign-library example)

;; CALLBACK-HEAVEN will also define a function (in the .c/.h files) with
;; the name set_function_index_group_<api-group-name>.  This function
;; will take in pointers to our defined lisp functions (in example.lisp)
;; and set-up the trampoline structure in C.  We need to inform CFFI of
;; its existence:
;;
(cffi:defcfun ("set_function_index_group_example" %set-function-index-group-example)
    :void
  (functions (:pointer (:pointer :void))))

(defun set-group-example-index ()
  (%set-function-index-group-example
   (c-space-translation-function-index *ctrans*)))

(set-group-example-index)

;; In lispcall.c we defined a function call_me_from_lisp, that will act as
;; our go-between.  Tell CFFI of its existence, and invoke it.
(cffi:defcfun ("call_me_from_lisp" %call-me-from-lisp) :void)

(let ((expected (format nil "Factorial ~A = ~A~%" 5 120))
      (actual (with-output-to-string (*standard-output*)
                (%call-me-from-lisp))))
  (cond ((string= actual expected)
         (write-line "Test succeeded.")
         (uiop:quit 0))
        (t
         (format t "~&Test failed.~@
                    Expected: ~S~@
                    Got:      ~S~%"
                 expected actual)
         (uiop:quit 1))))
