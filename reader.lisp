(defpackage :cl-kl-c2ffi/reader
  (:nicknames :c2ffi/reader)
  (:use #:cl)
  (:export :*c2ffi-bin-path*
           :*c2ffi-bin-params*
           :process-file))

(in-package :c2ffi/reader)

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

(defun parse-c2ffi-output (output processor)
  (declare (type string output)
           (type function processor))
  (with-input-from-string (s output)
    (loop for exp = (read-with-invert s)
          while exp
          do (funcall processor exp))))

(defun process-file (input-file processor)
  (declare (type pathname input-file)
           (type function processor))
  (parse-c2ffi-output (run-c2ffi input-file) processor))
