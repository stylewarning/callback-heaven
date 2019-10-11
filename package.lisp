;;;; package.lisp
;;;;
;;;; Copyright (c) 2015-2019 Robert Smith

(defpackage #:callback-heaven
  (:use #:cl)
  (:export
   #:define-api-group                   ; MACRO
   #:define-api-function                ; MACRO
   #:compute-c-space-translations       ; FUNCTION
   #:emit-library-files                 ; FUNCTION
   )
  (:documentation "Functionality to write and emit C compatible libraries of functions."))

