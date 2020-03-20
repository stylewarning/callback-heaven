;;;; callback-heaven.lisp
;;;;
;;;; Copyright (c) 2015-2019 Robert Smith

(in-package #:callback-heaven)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API GROUPS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; API groups are logical groups of functions callable from C. Each
;;; API group corresponds to a C header + file.

(defvar *api-groups* (make-hash-table :test 'eq)
  "A table mapping API group names (SYMBOLs) to API-GROUP structures.")

(defun api-group (group-name)
  "Return the API-GROUP associated with the name GROUP-NAME."
  (values (gethash group-name *api-groups*)))

(defun (setf api-group) (new-value group-name)
  (setf (gethash group-name *api-groups*) new-value))

(defstruct api-group
  "An API-GROUP is a named set of C-compatible Lisp functions. These functions are described by the API-FUNCTION structure."
  (name nil :type symbol :read-only t)
  ;; FUNCTION-TABLE is a hash table mapping symbols to API-FUNCTIONs.
  (function-table nil :type hash-table :read-only t))

(defun find-or-make-api-group (group-name)
  "Find the API group designated by the name GROUP-NAME, or make it if it doesn't exist."
  (let ((ag (api-group group-name)))
    (or ag (setf (api-group group-name)
                 (make-api-group :name group-name
                                 :function-table (make-hash-table :test 'eq))))))

(defun api-group-function (group function-name)
  "Given an API group GROUP, look up the function named FUNCTION-NAME. Return NIL if not found."
  (check-type group api-group)
  (check-type function-name symbol)
  (values (gethash function-name (api-group-function-table group))))

(defun (setf api-group-function) (new-value group function-name)
  (setf (gethash function-name (api-group-function-table group))
        new-value))

(defmacro define-api-group (name)
  "Declare the existence of the API group named NAME. Intended as a top-level form."
  (check-type name symbol)
  `(progn
     (find-or-make-api-group ',name)
     ',name))


;;;;;;;;;;;;;;;;;;;;;;;;;;; API FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; API functions are individually callable functions from C, but
;;; defined in Lisp.

(defstruct api-function
  "A representation of a Lisp function that is callable from C."
  ;; The Lisp name of the function, which can only be referenced recursively.
  (name          nil :type symbol               :read-only t)
  ;; The name of the callback.
  (callback-name nil :type symbol               :read-only t)
  ;; A pointer to the callback function.
  (pointer       nil :type cffi:foreign-pointer :read-only t)
  ;; The user-specified name of the function in the C world. By
  ;; default, this will make a reasonable attempt to generate a C
  ;; name, for instance, by changing #\- to #\_.
  (c-name        nil :type string               :read-only t)
  ;; The C return type, specified as a Lisp symbol.
  (return-type   nil                            :read-only t)
  ;; The C argument names and types, specified as Lisp pairs.
  (arguments     nil                            :read-only t))

(defun api-function-type (api-function)
  "Return the (C) function pointer type of the function API-FUNCTION."
  `(:function ,(api-function-return-type api-function)
              ,@(mapcar #'second (api-function-arguments api-function))))

(defmacro define-api-function ((name group-name &key c-name)
                               return-type (&rest args-and-types)
                               &body body)
  "Define an API function that is callable from C.

- NAME should be the Lisp name of the function.

- GROUP-NAME is the API group where this function lives.

- Optionally, C-NAME is the name of this function as seen by C.

- RETURN-TYPE is the CFFI return type of this function. Getting this incorrect is hazardous.

- ARGS-AND-TYPES are pairs of (ARGUMENT TYPE) specified as CFFI types.

For example:

  (define-api-function (square math-functions :c-name \"sq\") ((x :int))
    (ldb (byte 64 0) (* x x)))
"
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
                  #-sbcl
                  (,callback-name ,@args)
                  #+sbcl
                  (cffi:foreign-funcall-pointer
                   ;; Should we attempt to store this somewhere for
                   ;; extra efficiency?
                   (cffi:get-callback ',callback-name)
                   ()
                   ,@(mapcan #'reverse args-and-types)
                   ,return-type)))
           (declare (inline ,name)
                    (ignorable (function ,name)))
           ,@body))

       (setf (api-group-function (api-group ',group-name) ',name)
             (make-api-function :name ',name
                                :callback-name ',callback-name
                                :pointer (cffi:get-callback ',callback-name)
                                :c-name ,c-name
                                :return-type ',return-type
                                :arguments ',args-and-types))

       ;; Neither CCL nor LispWorks need an update here, since the
       ;; pointers seem to persist across redefinitions. It's not
       ;; unsafe to do it anyway, however.
       ;;
       ;; NOTE: This only updates EXISTING definitions. Any additional
       ;; ones will require modification of the C translation data
       ;; structure, as well as regeneration of the C libraries.
       (let ((ctrans (gethash (api-group ',group-name) *api-group-translations*)))
         (when ctrans
           (update-foreign-function-index ctrans)))

       ',name)))

;;;;;;;;;;;;;;;;;;;;;;; TRAMPOLINE GENERATION ;;;;;;;;;;;;;;;;;;;;;;;;

;;; API groups are translated into C files and their existence is kept
;;; track of by way of a "C space translation". We have to do this
;;; bookkeeping for the following reason. C doesn't have the Lisp
;;; functions defined; instead, it has stubs that must be patched
;;; in. The stubs look up the function pointer addresses in an array
;;; which Lisp fills out. Maintaining that array and filling it out is
;;; the role of the C-SPACE-TRANSLATION structure.

(defstruct c-space-translation
  "Manages the translation of an API-GROUP to realized functions in C space."
  ;; The API group for this translation.
  (api-group nil :type api-group :read-only t)
  ;; A vector of function names corresponding to the function pointers
  ;; of FUNCTION-INDEX.
  index-translations
  ;; The name of the function pointer array in C.
  (function-index-c-name nil :type string :read-only t)
  ;; A foreign array of function pointers corresponding to this API
  ;; group.
  (function-index nil :type cffi:foreign-pointer :read-only t))

(defun function-index-size-variable-name (ctrans)
  "The C variable name indicating the number of functions in the index."
  (format nil "~A_SIZE" (string-upcase (c-space-translation-function-index-c-name ctrans))))

(defun function-index-setter-function-name (ctrans)
  "The C function name used to patch in the function pointers."
  (format nil "set_~A" (c-space-translation-function-index-c-name ctrans)))

(defun foreign-function-index (api-group)
  "Generate an array (allocated with malloc) of function pointers for the API-GROUP which may be patched in.

Note that this memory is not further managed!"
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
  "Update the function pointers of CTRANS for any callbacks that have updated."
  (let* ((api-group (c-space-translation-api-group ctrans))
         (function-index (c-space-translation-function-index ctrans))
         (index-translations (c-space-translation-index-translations ctrans)))
    (loop :for i :from 0
          :for fname :across index-translations
          :for updated-ptr := (api-function-pointer (api-group-function api-group fname))
          :do (setf (cffi:mem-aref function-index ':pointer i) updated-ptr)
          :finally (return (values
                            function-index)))))

(defvar *api-group-translations* (make-hash-table :test 'eq)
  "A table mapping API-GROUP objects (by identity!) to their C space translations.")

(defun compute-c-space-translation (api-group)
  "Given an API group, compute C space translations for it."
  (multiple-value-bind (function-index index-translations)
      (foreign-function-index api-group)
    (setf (gethash api-group *api-group-translations*)
          (make-c-space-translation
           :api-group api-group
           :index-translations index-translations
           :function-index-c-name (format nil "function_index_group_~A"
                                          (cffi:translate-name-to-foreign (api-group-name api-group) nil))
           :function-index function-index))))

(defun emit-api-function-prototype (api-function stream)
  (flet ((format-arg (arg-and-type)
           (format nil "~A~:[~; ~]~A"
                   (type-name-to-foreign (second arg-and-type))
                   (%need-space-p arg-and-type)
                   (cffi:translate-name-to-foreign (first arg-and-type) nil))))
    (format stream "~A~:[ ~;~]~A(~{~A~^, ~})"
            (type-name-to-foreign (api-function-return-type api-function))
            (listp (api-function-return-type api-function))
            (api-function-c-name api-function)
            (mapcar #'format-arg (api-function-arguments api-function)))))

(defun emit-api-function-header (ctrans stream)
  ;; Emit all of the API prototypes.
  (loop :with api-group := (c-space-translation-api-group ctrans)
        :for fname :across (c-space-translation-index-translations ctrans)
        :for f := (api-group-function api-group fname)
        :do (terpri stream)
            (emit-api-function-prototype f stream)
            (write-char #\; stream)
            (terpri stream)))

(defun emit-api-function-definitions (ctrans stream &key
                                        (prefix  nil)
                                        (postfix nil))
  (let ((index-translations (c-space-translation-index-translations ctrans))
        (api-group (c-space-translation-api-group ctrans)))
    (loop :for i :from 0
          :for fname :across index-translations
          :for f := (api-group-function api-group fname)
          :do (terpri stream)
              (emit-api-function-prototype f stream)
              (format stream " {~%")
              (unless (eq :void (api-function-return-type f))
                (format stream "    ~A ret;~%" (type-name-to-foreign (api-function-return-type f))))
              (when prefix
                (if (functionp prefix)
                    (funcall prefix stream ctrans f)
                    (format stream "    ~A~%" prefix)))
              (format stream "    ~:[ret = ~;~]((~A)(~A[~D]))(~{~A~^, ~});~%"
                      (eq :void (api-function-return-type f))
                      (type-name-to-foreign (api-function-type f))
                      (c-space-translation-function-index-c-name ctrans)
                      i
                      (mapcar (lambda (name) (cffi:translate-name-to-foreign name nil))
                              (mapcar #'first (api-function-arguments f))))
              (when postfix
                (if (functionp postfix)
                    (funcall postfix stream ctrans f)
                    (format stream "    ~A~%" postfix)))
              (unless (eq :void (api-function-return-type f))
                (format stream "    return ret;~%"))
              (format stream "}~%~%"))))

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

(defun emit-includes (stream includes)
  (unless (endp includes)
    (format stream "~{#include ~A~^~%~}~%~%" includes)))

;;; Helper for EMIT-LIBRARY-FILES.
(defun emit-h-file-contents (ctrans stream &key extra-includes)
  (let ((guard (format nil "GROUP_~A_HEADER_GUARD"
                       (string-upcase
                        (cffi:translate-name-to-foreign
                         (api-group-name (c-space-translation-api-group ctrans))
                         nil)))))
    (format stream "#ifndef ~A~%" guard)
    (format stream "#define ~A~%~%" guard)
    (emit-includes stream (list* "<stdint.h>" "<stddef.h>" extra-includes))
    (emit-api-function-header ctrans stream)
    (format stream "~&~%~%#endif /* ~A */~%" guard)))

;;; Helper for EMIT-LIBRARY-FILES.
(defun emit-c-file-contents (ctrans stream h-file &key prefix postfix extra-includes)
  (emit-includes stream (cons (format nil "~S" (file-namestring h-file))
                              extra-includes))
  (emit-function-index-definition ctrans stream)
  (emit-api-function-definitions ctrans stream :prefix prefix :postfix postfix))



(defun emit-library-files (ctrans c-file h-file
                           &key (if-exists ':supersede)
                                (extra-c-file-includes '())
                                (extra-h-file-includes '())
                                (function-body-prefix nil)
                                (function-body-postfix nil))
  "Emit a header file H-FILE and a C file C-FILE for the C space translations CTRANS.

The C file may be compiled either as a shared library or as a part of a larger system. Its \"exports\" are in the header file.

* IF-EXISTS is an argument passed to OPEN.

* EXTRA-C-FILE-INCLUDES is a LIST of STRINGs indicating extra files that should be included in C-FILE.

* EXTRA-H-FILE-INCLUDES is a LIST of STRINGs indicating extra files that should be included in H-FILE.

* FUNCTION-BODY-PREFIX allows the caller to specify a prefix to be added to each API-FUNCTION definition emitted in C-FILE.
  * If a STRING, it is inserted verbatim at the beginning of each API-FUNCTION.
  * If a FUNCTION, it will be called with three arguments: 1) the STREAM on which to write the prefix, 2) the current C-SPACE-TRANSLATION, and 3) the current API-FUNCTION.
  * If NULL, no prefix will be emitted.

* FUNCTION-BODY-POSTFIX is just like FUNCTION-BODY-PREFIX, but for adding user code near the end of each API-FUNCTION, just before returning."
  (let ((c-file (pathname c-file))
        (h-file (pathname h-file)))
    (with-open-file (stream h-file :direction ':output
                                   :if-does-not-exist ':create
                                   :if-exists if-exists)
      (emit-h-file-contents ctrans stream :extra-includes extra-h-file-includes))

    (with-open-file (stream c-file :direction ':output
                                   :if-does-not-exist ':create
                                   :if-exists if-exists)
      (emit-c-file-contents ctrans stream h-file
                            :prefix function-body-prefix
                            :postfix function-body-postfix
                            :extra-includes extra-c-file-includes))

    (values c-file h-file)))
