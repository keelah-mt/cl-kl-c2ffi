(defpackage :cl-kl-c2ffi/translator
  (:nicknames :c2ffi/translator)
  (:use #:cl)
  (:import-from :trivia
                :match)
  (:import-from :serapeum/bundle
                :match-of)
  (:import-from :alexandria
                :define-constant
                :curry)
  (:import-from :cl-change-case
                :param-case)
  (:local-nicknames (:p :c2ffi/parser)
                    (:%r :c2ffi/reader))
  (:export :translate
           :translate-file
           :default-translate-name))

(in-package :c2ffi/translator)

(defun set-name-case (str-name readtable)
  (ecase (readtable-case readtable)
    (:upcase (string-upcase str-name))
    (:downcase (string-downcase str-name))
    (:preserve str-name)
    ;; TODO: ehm, so... what to do with this??? 
    (:invert str-name)))

(defun default-translate-name (fname ftype-tag)
  (declare (type p:form-name fname)
           (type p:form-type-tag ftype-tag))
  (declare (ignore ftype-tag))
  (match-of p:form-name fname
    ((p:fn-string value)
     (format nil "~A~A" (if (uiop:string-prefix-p "_" value) "%" "") (param-case value)))
    ((p:fn-keyword value)
     (param-case value))
    ((p:fn-id value)
     (format nil "%UNNAMED-ID-~D" value))))

;; TODO: this ignores arrays for now
(defun maybe-resolve-alias (registry form name-translator full-resolve)
  (declare (type p:form-def form))
  (assert (typep (p:form-def-data form) 'p:ft-typedef-alias)
          ()
          "FORM-ID must point to a FT-TYPEDEF-ALIAS instance, received: ~S" (p:form-def-data form))
  (labels ((resolve (form ptrs nesting)
             ;; go down the chain of typedefs and see if we need/can resolve it
             (declare (type p:form-def form)
                      (type integer ptrs))
             (let* ((ftype (p:form-def-data form))
                    (fname (p:form-def-name form)))
               (match-of p:form-type ftype
                 ((p:ft-typedef-alias ptr-counter target _)
                  (if (typep target 'p:atom-type)
                      (when (or (= nesting 0) full-resolve)
                        (list target (+ ptr-counter ptrs)))

                      (let ((target-form (p:registry-find-form registry target)))
                        (or (resolve target-form (+ ptr-counter ptrs) (1+ nesting))
                            ;; if resolve failed, fallback to top-level definition
                            (when (= nesting 0)
                              (let ((ft-typename
                                      (p:get-ft-typename (p:form-def-data target-form))))
                                (list
                                 (funcall name-translator
                                          (p:form-def-name target-form)
                                          ft-typename)
                                 (+ ptr-counter ptrs)
                                 ft-typename)))))))

                 ((p:ft-struct size)
                  ;; when alias points directly to struct, output the struct,
                  ;; to workaround defctype broken logic:
                  ;; https://github.com/cffi/cffi/issues/295
                  ;; basically this is why this whole mess of a project exists :-D
                  ;; plus we also don't ouput defctype for enums, because this also
                  ;; breaks translation of enum values between keyword<->int
                  ;; same as in that bug report above, I also don't know where to ask anything
                  ;; so I am just trying to survive here...
                  ;; yeah, so and otherwise leave the alias as is, if it points to an opaque
                  ;; struct because in that case we don't care anyway, there is nothing to translate
                  (when (or (/= 0 size) (= nesting 1))
                    (list `(:struct ,(funcall name-translator fname 'p:ft-struct))
                          ptrs
                          'p:ft-struct)))

                 (_ nil)))))
    (resolve form 0 0)))

(defun translate (registry &key package
                                (name-translator #'default-translate-name)
                                (resolve-aliases nil))
  (declare (type p:form-registry registry))
  (assert (packagep package) (package)
          "Package parameter must be a package: ~S" package)
  (labels ((with-pointers (ptr-count value)
             (if (> ptr-count 0)
                 `(:pointer ,(with-pointers (1- ptr-count) value))
                 value))

           (translator (fname ftype-tag)
             (with-standard-io-syntax
               (let ((kwp (typep fname 'p:fn-keyword)))
                 (intern (set-name-case
                          (funcall (or name-translator #'default-translate-name) fname ftype-tag)
                          *readtable*)
                         (if kwp :keyword package)))))

           (resolve-form (form)
             (let ((fname (p:form-def-name form))
                   (ftype (p:form-def-data form)))
               (match-of p:form-type ftype
                 (p:ft-typedef-alias
                  (let ((resolved-data
                          (maybe-resolve-alias registry
                                               form
                                               #'translator
                                               resolve-aliases)))
                    ;; TODO: switch translator to other tags
                    ;; there is no e.g. field type in form-types or ret-type/args
                    ;; NOTE: don't emit defctype for enums, it seems they break
                    ;; value translation and add absolutely nothing...
                    (unless (eq 'p:ft-enum (third resolved-data))
                      `(,(translator fname 'p:ft-struct)
                        ,(with-pointers (second resolved-data)
                           (first resolved-data))))))
                 ;; TODO: at least this can be an atom
                 (_ `(,fname ,ftype))))))
    (remove-if
     #'null
     (mapcar (lambda (fid)
               (let* ((form (p:registry-find-form registry fid))
                      (form-name (p:form-def-name form)))
                 (match-of p:form-type (p:form-def-data form)
                   ;; const don't need cffi, so we define them with alexandria
                   ((p:ft-const _ value)
                    `(define-constant ,(translator form-name 'p:ft-const) ,value))

                   ;; enums are simple int maps, output as is
                   ((p:ft-enum members)
                    `(cffi:defcenum ,(translator form-name 'p:ft-enum)
                       ,@(mapcar (lambda (m)
                                   (list (translator (first m) 'p:ft-enum)
                                         (second m)))
                                 members)))

                   ;; skip defctype for enums to make translation work,
                   ;; that should be safe she said. 
                   (p:ft-typedef-alias
                    (let ((type-form (resolve-form form)))
                      (when type-form
                        `(cffi:defctype ,@type-form))))

                   ;; in structs resolve every field pointing to a non-opaque struct
                   ;; the rest can stay as is (subject to :resolve-aliases)
                   ((p:ft-struct size fields)
                    (let ((decl `(cffi:defcstruct (,(translator form-name 'p:ft-struct)
                                                   :size ,size)))
                          (fdef (mapcar (lambda (f)
                                          (let ((fform (first f))
                                                (fparams (second f)))
                                            (append (resolve-form fform)
                                                    (list :offset (getf fparams :offset)))))
                                        fields)))
                      (if fdef (append decl fdef) decl)))

                   ;; similar to struct in terms of type resolution for ret/arg values
                   ((p:ft-function ret-type args)
                    (let ((ret-type (second (resolve-form ret-type)))
                          (arg-def (mapcar (lambda (a)
                                             (resolve-form a)) args)))
                      `(cffi:defcfun
                           (,(translator form-name 'p:ft-function)
                            ,(p:form-name->string form-name))
                           ,ret-type
                         ,@arg-def))))))
             (p:registry-ordered-forms registry)))))

(defun translate-file (input-filename output-filename
                       &key package-designator
                            library-name
                            library-spec
                            (name-translator #'default-translate-name)
                            (resolve-aliases nil))
  (declare (type (not null) package-designator))
  (let ((package (or (find-package package-designator)
                     (make-package package-designator :use '())))
        (registry (p:make-form-registry)))
    (%r:process-file input-filename
                     (lambda (form)
                       (p:parse-form registry form :register t)))
    (with-open-file (output output-filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((translated (translate registry
                                     :package package
                                     :name-translator name-translator
                                     :resolve-aliases resolve-aliases))
              (*print-pretty* t)
              (*print-right-margin* 120)
              (*print-miser-width* 120)
              (*print-circle* nil)
              (*package* package))
          (write `(defpackage ,package-designator (:use)) :stream output)
          (terpri output)
          (write `(in-package ,package-designator) :stream output)
          (terpri output)
          (when (and library-name library-spec)
            (let ((lib-name (alexandria:format-symbol package "~A" library-name)))
              (write `(cffi:define-foreign-library ,lib-name ,@library-spec) :stream output)
              (terpri output)
              (write `(cffi:use-foreign-library ,lib-name) :stream output)
              (terpri output)))
          (terpri output)
          (dolist (form translated)
            (write form :stream output)
            (terpri output)
            (terpri output)))))))
