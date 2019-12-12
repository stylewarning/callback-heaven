;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:callback-heaven)

(defparameter *simple-type-name-translations*
  '((:pointer "void*")

    (:char "char")
    (:unsigned-char "unsigned char")
    (:uchar "unsigned char")

    (:short "short")
    (:unsigned-short "unsigned short")
    (:ushort "unsigned short")
    
    (:int "int")
    (:unsigned-int "unsigned int")
    (:uint "unsigned int")
    
    (:long "long")
    (:unsigned-long "unsigned long")
    (:ulong "unsigned long")
    
    #-cffi-sys::no-long-long
    (:long-long "long long")
    #-cffi-sys::no-long-long
    (:llong "long long")
    #-cffi-sys::no-long-long
    (:unsigned-long-long "unsigned long long")
    #-cffi-sys::no-long-long
    (:ullong "unsigned long long")
    
    (:float "float")
    (:double "double")
    #+scl
    (:long-double "long double")
    
    (:void "void")
    
    (:int8 "int8_t")
    (:uint8 "uint8_t")

    (:int16 "int16_t")
    (:uint16 "uint16_t")

    (:int32 "int32_t")
    (:uint32 "uint32_t")
    
    (:int64 "int64_t")
    (:uint64 "uint64_t")
    
    ;; XXX: Fixme?
    (:boolean "int")
    (:string "char*")))

(defun %need-space-p (name)
  (and (second name)
       (not (typep (second name) 'list))
       (not (eq ':pointer (second name)))))

(defun type-name-to-foreign (name)
  (labels ((simple-name-p (name)
             (symbolp name))
           
           (lookup-simple-type (name)
             (let ((translation (assoc name *simple-type-name-translations*)))
               (and translation
                    (second translation)))))
    (cond
      ((simple-name-p name) (or (lookup-simple-type name)
                                (cffi:translate-name-to-foreign name nil)))
      ((listp name) (case (first name)
                      ((:struct) (format nil "struct ~A"
                                         (cffi:translate-name-to-foreign (second name) nil)))
                      ((:pointer) (format nil "~A~:[~; ~]*"
                                          (type-name-to-foreign (second name))
                                          (%need-space-p name)))
                      ((:function)
                       (destructuring-bind (return-type &rest arg-types)
                           (rest name)
                         (format nil "~A (*)(~{~A~^, ~})"
                                 (type-name-to-foreign return-type)
                                 (mapcar #'type-name-to-foreign arg-types))))
                      (otherwise (error "Unknown foreign type ~S" name))))
      (t (error "Unknown foreign type ~S" name)))))
