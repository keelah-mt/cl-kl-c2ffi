(defpackage :cl-kl-c2ffi/test/suite
  (:nicknames :c2ffi/test/suite)
  (:use #:cl #:fiveam)
  (:local-nicknames (#:%p #:c2ffi/parser)
                    (#:%t #:cl-kl-c2ffi)
                    (#:%c #:coalton)))

(in-package :c2ffi/test/suite)

(defun run-parser (inputs)
  (let ((context (%t:cl-make-empty-context)))
    (loop for exp in inputs
          do
             (handler-case
                 (setq context (%t:cl-feed-input context exp))
               (error (c)
                 (format t "~%>>> WARNING: failed to feed input: ~A~%" c))))
    context))

(defun find-result (name context)
  (handler-case
      (%c:coalton (%p:get-form-string (%c:lisp %c:String () name)
                                      (%c:lisp %p:ParserContext () context)))
    (error (c)
      (format t "~%>>> WARNING: cannot find result: ~A~%" c))))

(defun resolve-form (name context)
  (handler-case
      (%c:coalton
       (%c:let ((form (%p:get-find-form (%c:lisp %c:String () name)
                                        (%c:lisp %p:ParserContext () context))))
         (%p:get-resolve-form-string form
                                     (%c:lisp %p:ParserContext () context))))
    (error (c)
      (format t "~%>>> WARNING: cannot resolve-kind: ~A~%" c))))

(defun convert-form (name context)
  (let ((package (find-package :c2ffi/test/suite)))
    (handler-case
        (%c:coalton
         (%c:let ((form (%p:get-find-form (%c:lisp %c:String () name)
                                          (%c:lisp %p:ParserContext () context))))
           (%p:get-convert-form-string form
                                       (%c:lisp %p:Package () package)
                                       (%c:lisp %p:ParserContext () context))))
      (error (c)
        (format t "~%>>> WARNING: cannot resolve-kind: ~A~%" c)))))

(def-suite c2ffi-test-all
  :description "Test cl-kl-c2ffi system")

(def-suite parse-forms
  :description "Test parsing functionality of the c2ffi data"
  :in c2ffi-test-all)

(def-suite translate-forms
  :description "Test form translation into cffi format"
  :in c2ffi-test-all)

