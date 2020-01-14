;;;; package.lisp
;;;;
;;;; Copyright (c) 2015-2019 Robert Smith

(defpackage #:callback-heaven
  (:use #:cl)
  (:export
   #:define-api-group                   ; MACRO
   #:define-api-function                ; MACRO
   #:compute-c-space-translation        ; FUNCTION
   #:c-space-translation-function-index ; FUNCTION
   #:emit-library-files                 ; FUNCTION
   #:api-group
   )
  (:documentation "Functionality to write and emit C compatible libraries of functions."))

