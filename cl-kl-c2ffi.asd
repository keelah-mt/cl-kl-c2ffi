(defsystem "cl-kl-c2ffi"
  :description "cl-kl-c2ffi: c2ffi => cffi translator"
  :version "0.0.1"
  :author "Kira Verhovyh <git@keelah.cc>"
  :license "AGPLv3"
  :source-control (:git "git@github.com:keelah-mt/cl-kl-c2ffi.git")
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :depends-on (#:cffi #:alexandria #:serapeum #:coalton #:cl-kl-c2ffi/parser)
  :serial t
  :components ((:file "translator")
               ;;(:file "asdf")
               )
  :in-order-to ((test-op (test-op :cl-kl-c2ffi/test))))

(defsystem "cl-kl-c2ffi/parser"
  :description "clkl-c2ffi: c2ffi input parser"
  :depends-on (#:cl-change-case #:coalton #:named-readtables)
  :pathname "parser"
  :serial t
  :components ((:file "parser-package")
               (:file "input-view")
               (:file "typedefs")
               (:file "into-str")
               (:file "input-parsers")
               (:file "form-parsers")
               (:file "run-parser")))

(defsystem "cl-kl-c2ffi/test"
  :description "cl-kl-c2ffi: tests"
  :depends-on (#:cffi #:trivial-backtrace #:fiveam #:cl-kl-c2ffi)
  :pathname "test"
  :serial t
  :components ((:file "suite")
               (:file "parse-typedef")
               ;; (:file "parse-const")
               ;; (:file "parse-enum")
               ;; (:file "parse-typedef")
               ;; (:file "parse-struct")
               ;; (:file "parse-function")
               ;; (:file "process-input")
               )
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :c2ffi-test-all :cl-kl-c2ffi/test/suite))))
