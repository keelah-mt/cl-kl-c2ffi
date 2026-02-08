(defpackage :cl-kl-c2ffi/asdf
  (:nicknames :c2ffi/asdf)
  (:use #:cl)
  (:export :c2ffi-lisp-file
           :c2ffi-spec-op
           :c2ffi-lisp-op))

(in-package :c2ffi/asdf)

(defclass c2ffi-lisp-file (asdf:cl-source-file)
  ((type :initform "h")
   (package :initarg :package
            :initform (error ":package is required"))
   (foreign-library-name :initarg :foreign-library-name
                         :initform nil)
   (foreign-library-spec :initarg :foreign-library-spec
                         :initform nil))
  (:documentation "A C header that produces a platform-specific spec file with c2ffi,
then a lisp file for the current platform."))

(defclass c2ffi-spec-op (asdf:downward-operation) ())

(defclass c2ffi-lisp-op (asdf:downward-operation asdf:selfward-operation)
  ((asdf:selfward-operation :initform 'c2ffi-spec-op :allocation :class)))

;; --- STAGE 1: C Header -> Spec ---

(defmethod asdf:input-files ((o c2ffi-spec-op) (c c2ffi-lisp-file))
  (list (asdf:component-pathname c)))

(defmethod asdf:output-files ((o c2ffi-spec-op) (c c2ffi-lisp-file))
  (list (make-pathname :type "spec" :defaults (asdf:component-pathname c))))

(defmethod asdf:perform ((o c2ffi-spec-op) (c c2ffi-lisp-file))
  (let* ((input (asdf:component-pathname c))
         (output (first (asdf:output-files o c))))
    (format t "~&; Running c2ffi on ~A => ~A~%" input output)
    (with-slots (package foreign-library-name foreign-library-spec) c
      (c2ffi/translator:translate-file input
                                       output
                                       :package-designator package
                                       :library-name foreign-library-name
                                       :library-spec foreign-library-spec)))
  (unless (probe-file (first (asdf:output-files o c)))
    (error "c2ffi failed to produce output for ~A" c)))

;; --- STAGE 2: Spec -> Lisp ---

(defmethod asdf:input-files ((o c2ffi-lisp-op) (c c2ffi-lisp-file))
  (asdf:output-files 'c2ffi-spec-op c))

(defmethod asdf:output-files ((o c2ffi-lisp-op) (c c2ffi-lisp-file))
  (list (make-pathname :type "lisp" :defaults (asdf:component-pathname c))))

;; Here we assume the .spec file has the same name as the component
(defmethod asdf:perform ((o c2ffi-lisp-op) (c c2ffi-lisp-file))
  (let* ((input (first (asdf:output-files 'c2ffi-spec-op c)))
         (output (first (asdf:output-files o c))))
    (format t "~&; Generating Lisp from ~A => ~A~%" input output)
    (with-open-file (s output
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      ;; TODO: doesn't do anything useful atm, as this doesn't suppport multiple platforms
      (uiop:copy-file input output))))

;; --- STAGE 3: Integration with Standard Load ---

(defmethod asdf:component-depends-on ((o asdf:compile-op) (c c2ffi-lisp-file))
  `((c2ffi-lisp-op ,c) ,@(call-next-method)))

(defmethod asdf:input-files ((o asdf:compile-op) (c c2ffi-lisp-file))
  (asdf:output-files 'c2ffi-lisp-op c))

(setf (find-class 'asdf::c2ffi-lisp-file) (find-class 'c2ffi-lisp-file))
