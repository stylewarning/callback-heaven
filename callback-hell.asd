;;;; callback-hell.asd
;;;;
;;;; Copyright (c) 2015 Robert Smith <quad@symbo1ics.com>

(asdf:defsystem #:callback-hell
  :description "A framework for calling Lisp from C."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:cffi #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "callback-hell")))

