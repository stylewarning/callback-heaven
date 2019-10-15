;;;; callback-heaven-examples.asd
;;;;
;;;; Copyright (c) 2015 Robert Smith <quad@symbo1ics.com>

(asdf:defsystem #:callback-heaven-examples
  :description "A framework for calling Lisp from C."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE.txt)"
  :depends-on (#:cffi #:alexandria #:callback-heaven)
  :serial t
  :components ((:module "examples"
                :serial t
                :components
                ((:file "package")
                 (:file "example")))))

