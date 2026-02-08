(defpackage :cl-kl-c2ffi/test/suite
  (:nicknames :c2ffi/test/suite)
  (:use :cl :fiveam)
  (:local-nicknames (:%p :c2ffi/parser)
                    (:%t :c2ffi/translator)))

(in-package :c2ffi/test/suite)

(def-suite c2ffi-test-all
  :description "Test cl-kl-c2ffi system")

(def-suite parse-typedef-forms
  :description "Test parsing functionality of the c2ffi typedef data"
  :in c2ffi-test-all)

(def-suite parse-struct-forms
  :description "Test parsing functionality of the c2ffi struct data"
  :in c2ffi-test-all)

(def-suite parse-enum-forms
  :description "Test parsing functionality of the c2ffi enum data"
  :in c2ffi-test-all)

(def-suite parse-const-forms
  :description "Test parsing functionality of the c2ffi const data"
  :in c2ffi-test-all)

(def-suite parse-function-forms
  :description "Test parsing functionality of the c2ffi function data"
  :in c2ffi-test-all)

(def-suite process-input
  :description "Test input is processed correctly and all forms are registered"
  :in c2ffi-test-all)

(def-suite process-file
  :description "Test loading .h files and generating cffi output"
  :in c2ffi-test-all)

(defun make-registry (fill-forms)
  (let ((registry (%p:make-form-registry)))
    (dolist (f fill-forms)
      (%p:registry-add-form registry (%p:form-def-name f) (%p:form-def-data f)))
    registry))


