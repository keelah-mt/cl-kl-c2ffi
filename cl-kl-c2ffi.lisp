(defpackage :cl-kl-c2ffi
  (:use #:cl)
  (:import-from :alexandria
                :define-constant
                :format-symbol
                :curry)
  (:import-from :cl-change-case
                :param-case)
  (:local-nicknames (#:%c #:coalton)
                    (#:%p #:c2ffi/parser))
  (:export
   :cl-feed-input
   :cl-make-empty-context
   :cl-translate-context
   :parse-file
   :translate-file))

(in-package :cl-kl-c2ffi)

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

(defun cl-feed-input (context exp)
  (%c:coalton
   (%c:let ((ctx (%c:lisp %p:ParserContext () context))
            (view (%c:lisp %p:InputView () exp)))
     (%p:feed-input ctx view))))

(defun cl-make-empty-context ()
  (%c:coalton (%p:make-empty-context)))

(defun cl-find-form (lookup context)
  (%c:coalton (%p:get-find-form (%c:lisp %c:String () lookup)
                                (%c:lisp %p:ParserContext () context))))

;; DEBUG: test glfw
(defun cl-glfw-name-translator (str)
  (let ((fixed (if (alexandria:starts-with-subseq "GLFW" str)
                   (concatenate 'string "GLFW-" (subseq str 4))
                   str)))
    (%p:to-param-case fixed)))

(%c:coalton-toplevel
  (%c:define (glfw-name-translator s)
    (%c:lisp %c:String (s)
      (cl-glfw-name-translator s)))
)

(defun cl-convert-form (form package context name-translator)
  (%c:coalton (%p:get-convert-form (%c:lisp %p:RankedForm () form)
                                   (%c:lisp %p:Package () package)
                                   (%c:lisp %p:NameTranslator () name-translator)
                                   (%c:lisp %p:ParserContext () context))))

(defun cl-translate-context (context package name-translator)
  (%c:coalton
   (%p:translate
    (%c:lisp %p::ParserContext () context)
    (%c:lisp %p:Package () package)
    (%c:lisp %p:NameTranslator () name-translator))))

(defun cl-get-translation (tr)
  (%c:coalton (%p:get-translation (%c:lisp %p:TranslatorResult () tr))))

(defun parse-file (input-file)
  (declare (type pathname input-file))
  (let ((context (cl-make-empty-context))
        (err-count 0))
    (with-input-from-string (s (run-c2ffi input-file))
      (loop for exp = (read-with-invert s)
            while exp
            do
               ;; (format t ">>> ~S ~%" exp)
               (handler-case
                   (setq context (cl-feed-input context exp))
                 (error (c)
                   (incf err-count)
                   (format t ">>> FAILED TO PARSE: ~S, ~S~%" exp c)
                   ;; (error c)
                   ))))
    (format t ">>> TOTAL ERRORS: ~D~%" err-count)
    context))

(defun load-translation (input-filename package name-translator)
  (cl-get-translation (cl-translate-context (parse-file input-filename)
                                            package
                                            name-translator)))

(defun translate-file (input-filename output-filename
                       &key package-designator
                         library-name
                         library-spec
                         (name-translator %p:to-param-case))
  (declare (type (not null) package-designator))
  (let* ((package (or (find-package package-designator)
                      (make-package package-designator :use '())))
         (translated (load-translation input-filename package name-translator)))
    (with-open-file (output output-filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-pretty* t)
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
            (write form :stream output :escape nil)
            (terpri output)
            (terpri output)))))))

