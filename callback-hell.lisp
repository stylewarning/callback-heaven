;;;; callback-hell.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith <quad@symbo1ics.com>

(in-package #:callback-hell)

(defvar *api-groups* (make-hash-table :test 'eq))

(defun api-group (group-name)
  (values (gethash group-name *api-groups*)))

(defun (setf api-group) (new-value group-name)
  (setf (gethash group-name *api-groups*) new-value))

(defstruct api-group
  name
  function-table)

(defun find-or-make-api-group (group-name)
  (let ((ag (api-group group-name)))
    (or ag (setf (api-group group-name)
                 (make-api-group :name group-name
                                 :function-table (make-hash-table :test 'eq))))))

(defun api-group-function (group function-name)
  (check-type group api-group)
  (values (gethash function-name (api-group-function-table group))))

(defun (setf api-group-function) (new-value group function-name)
  (setf (gethash function-name (api-group-function-table group))
        new-value))


(defmacro define-api-group (name)
  `(progn
     (find-or-make-api-group ',name)
     ',name))

(defstruct api-function
  name
  callback-name
  pointer
  c-name
  return-type
  arguments)

(defun api-function-type (api-function)
  `(:function ,(api-function-return-type api-function)
              ,@(mapcar #'second (api-function-arguments api-function))))

(defmacro define-api-function ((name group-name &key c-name)
                               return-type (&rest args-and-types)
                               &body body)
  (check-type name symbol)
  (check-type group-name symbol)
  (check-type c-name (or null string))
  (when (null c-name)
    (setf c-name (cffi:translate-name-to-foreign name nil)))
  
  (let* ((api-group (api-group group-name))
         (current-api-function (api-group-function api-group name))
         (callback-name (if current-api-function
                            (api-function-callback-name current-api-function)
                            (gensym (symbol-name name))))
         (args (mapcar #'first args-and-types)))
    `(progn
       (cffi:defcallback ,callback-name ,return-type ,args-and-types
         ;; Ensure we can do a recursive call.
         (flet ((,name ,args
                  (,callback-name ,@args)))
           (declare (inline ,name))
           ,@body))
       
       (setf (api-group-function (api-group ',group-name) ',name)
             (make-api-function :name ',name
                                :callback-name ',callback-name
                                :pointer (cffi:get-callback ',callback-name)
                                :c-name ,c-name
                                :return-type ',return-type
                                :arguments ',args-and-types))
       
       ;; TODO? Maybe need to call update-foreign-function-index. In
       ;; CCL, the pointers seem to persist across redefinitions.
       
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emission ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct c-space-translation
  api-group
  index-translations
  function-index-c-name
  function-index)

(defun function-index-size-variable-name (ctrans)
  (format nil "~A_SIZE" (string-upcase (c-space-translation-function-index-c-name ctrans))))

(defun function-index-setter-function-name (ctrans)
  (format nil "set_~A" (c-space-translation-function-index-c-name ctrans)))

(defun foreign-function-index (api-group)
  (let* ((functions (api-group-function-table api-group))
         (function-count (hash-table-count functions))
         (function-index (cffi:foreign-alloc ':pointer
                                             :initial-element (cffi:null-pointer)
                                             :count function-count))
         (index-translations (make-array function-count)))
    (loop :for i :below function-count
          :for name :being :the :hash-keys :of functions :using (hash-value f)
          :do (setf (cffi:mem-aref function-index ':pointer i) (api-function-pointer f)
                    (aref index-translations i) name)
          :finally (return (values
                            function-index
                            index-translations)))))

(defun update-foreign-function-index (ctrans)
  (let* ((api-group (c-space-translation-api-group ctrans))
         (function-index (c-space-translation-function-index ctrans))
         (index-translations (c-space-translation-index-translations ctrans)))
    (loop :for i :from 0
          :for fname :across index-translations
          :for updated-ptr := (api-function-pointer (api-group-function api-group fname))
          :do (setf (cffi:mem-aref function-index ':pointer i) updated-ptr)
          :finally (return (values
                            function-index)))))

(defun compute-c-space-translation (api-group)
  (multiple-value-bind (function-index index-translations)
      (foreign-function-index api-group)
    (make-c-space-translation
     :api-group api-group
     :index-translations index-translations
     :function-index-c-name (format nil "function_index_group_~A"
                                    (cffi:translate-name-to-foreign (api-group-name api-group) nil))
     :function-index function-index)))


(defvar *default-includes* (list "<stdint.h>" "<stddef.h>"))

(defun emit-includes (stream)
  (format stream "~{#include ~A~^~%~}~%~%" *default-includes*))

(defun emit-api-function-prototype (api-function stream)
  (flet ((format-arg (arg-and-type)
           (format nil "~A ~A"
                   (type-name-to-foreign (second arg-and-type))
                   (cffi:translate-name-to-foreign (first arg-and-type) nil))))
    (format stream "~A ~A(~{~A~^, ~})"
            (type-name-to-foreign (api-function-return-type api-function))
            (api-function-c-name api-function)
            (mapcar #'format-arg (api-function-arguments api-function)))))

(defun emit-api-function-header (ctrans stream)
  ;; Emit the function index setter.
  (terpri stream)
  (format stream "void ~A(void** functions);~%"
          (function-index-setter-function-name ctrans))
  
  ;; Emit all of the API prototypes.
  (loop :with api-group := (c-space-translation-api-group ctrans)
        :for fname :across (c-space-translation-index-translations ctrans)
        :for f := (api-group-function api-group fname)
        :do (terpri stream)
            (emit-api-function-prototype f stream)
            (write-char #\; stream)
            (terpri stream)))

(defun emit-api-function-definitions (ctrans stream)
  (let ((index-translations (c-space-translation-index-translations ctrans))
        (api-group (c-space-translation-api-group ctrans)))
    (loop :for i :from 0
          :for fname :across index-translations
          :for f := (api-group-function api-group fname)
          :do (terpri stream)
              (emit-api-function-prototype f stream)
              (write-string " {" stream)
              (terpri stream)
              (format stream "~4T")
              (unless (eq :void (api-function-return-type f))
                (write-string "return " stream))
              (format stream "((~A)(~A[~D]))(~{~A~^, ~});"
                      (type-name-to-foreign (api-function-type f))
                      (c-space-translation-function-index-c-name ctrans)
                      i
                      (mapcar (lambda (name) (cffi:translate-name-to-foreign name nil))
                              (mapcar #'first (api-function-arguments f))))
              (terpri stream)
              (write-string "}" stream)
              (terpri stream)
              (terpri stream))))

(defun emit-function-index-definition (ctrans stream)
  (let ((idx-var (c-space-translation-function-index-c-name ctrans))
        (size-var (function-index-size-variable-name ctrans)))
    (format stream "~&#define ~A ~D~%~%"
            size-var
            (length (c-space-translation-index-translations ctrans)))
    (format stream "static void **~A;~%~%"
            idx-var)
    (format stream "void ~A(void **functions) {~%~
                    ~4T~A = functions;~%~
                    }~%~%"
            (function-index-setter-function-name ctrans)
            idx-var)))

(defun emit-h-file-contents (ctrans stream)
  (let ((guard (format nil "GROUP_~A_HEADER_GUARD"
                       (string-upcase
                        (symbol-name
                         (api-group-name
                          (c-space-translation-api-group ctrans)))))))
    (format stream "#ifndef ~A~%" guard)
    (format stream "#define ~A~%~%" guard)
    (emit-includes stream)
    (emit-api-function-header ctrans stream)
    (format stream "~&~%~%#endif /* ~A */" guard)))

(defun emit-c-file-contents (ctrans stream)
  (emit-function-index-definition ctrans stream)
  (emit-api-function-definitions ctrans stream))



(defun emit-library-files (ctrans c-file h-file &key (if-exists ':supersede))
  (let ((c-file (pathname c-file))
        (h-file (pathname h-file)))
    (with-open-file (stream h-file :direction ':output
                                   :if-does-not-exist ':create
                                   :if-exists if-exists)
      (emit-h-file-contents ctrans stream))

    (with-open-file (stream c-file :direction ':output
                                   :if-does-not-exist ':create
                                   :if-exists if-exists)
      (format stream "#include \"~A\"~%~%" (file-namestring h-file))
      (emit-c-file-contents ctrans stream))
    
    (values c-file h-file)))
