;;;; callback-heaven.asd
;;;;
;;;; Copyright (c) 2015 Robert Smith

(asdf:defsystem #:callback-heaven
  :description "A framework for calling Lisp from C."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:cffi #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "callback-heaven")))

