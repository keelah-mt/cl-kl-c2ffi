(defpackage :cl-kl-c2ffi/translator
  (:nicknames :c2ffi/translator)
  (:use #:cl)
  (:import-from :alexandria
                :define-constant
                :format-symbol
                :curry)
  (:import-from :cl-change-case
                :param-case)
  (:local-nicknames (#:%c #:coalton)
                    (#:%p #:c2ffi/parser)))

(in-package :c2ffi/translator)

(defparameter *c2ffi-bin-path* "/usr/bin/c2ffi"
  "Path to c2ffi binary.")

(defparameter *c2ffi-bin-params* '("-D" "sexp" "--fail-on-error" "--warn-as-error")
  "Default params to c2ffi binary.")

(defun run-c2ffi (input-file)
  (declare (type pathname input-file))
  (multiple-value-bind (output error exit)
      (uiop:run-program (cons *c2ffi-bin-path* `(,@*c2ffi-bin-params* ,(namestring input-file)))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (when (/= 0 exit)
      (error "c2ffi exited with code ~A.~% ~A" exit error))
    output))

;; I knew it is a trap...
;; turns out reading sexp format gives major headache in handling string case...
;; JSON next time!

(defun read-with-invert (stream)
  (let ((*readtable* (copy-readtable nil)))
    ;; this magic :invert case somehow preserves camelCase
    ;; and that's what we need to convert it into param-case later
    (setf (readtable-case *readtable*) :invert)
    (read stream nil)))

(defun parse-file (input-file)
  (declare (type pathname input-file))
  (let ((context (%c:coalton (%p::make-empty-context)))
        (err-count 0))
    (with-input-from-string (s (run-c2ffi input-file))
      (loop for exp = (read-with-invert s)
            while exp
            do
               (format t ">>> ~S ~%" exp)
               (handler-case
                    (setq context (%c:coalton
                                   (%c:let ((ctx (%c:lisp %p::ParserContext () context))
                                            (view (%c:lisp %p::InputView () exp)))
                                     (%p::feed-input ctx view))))
                 (error (c)
                   (incf err-count)
                   (format t ">>> FAILED TO PARSE: ~S, ~S~%" exp c)))))
    (format t ">>> TOTAL ERRORS: ~D~%" err-count)
    context))

(defun translate-file (input-filename output-filename
                       &key package-designator
                         library-name
                         library-spec)
  (declare (type (not null) package-designator))
  (let* ((package (or (find-package package-designator)
                      (make-package package-designator :use '())))
         (context (parse-file input-filename))
         (translated (%c:coalton (%p:translate
                                  (%c:lisp %p::ParserContext () context)
                                  (%c:lisp coalton:String () package-designator)))))
    (with-open-file (output output-filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (let (
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
            (let ((lib-name (format-symbol package "~A" library-name)))
              (write `(cffi:define-foreign-library ,lib-name ,@library-spec) :stream output)
              (terpri output)
              (write `(cffi:use-foreign-library ,lib-name) :stream output)
              (terpri output)))
          (terpri output)
          (dolist (form translated)
            (write form :stream output)
            (terpri output)
            (terpri output)))))))

