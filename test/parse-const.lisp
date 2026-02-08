(in-package :cl-kl-c2ffi/test/suite)
(in-suite parse-const-forms)

;; NOTE: only scalar definitions are currently supported

(test should-parse-simple-const
  (let ((input '(const my_const :int 10))
        (expect (%p::form-def -1
                              (%p::fn-string "MY_CONST")
                              (%p::ft-const :int 10))))
    (is (equalp expect (%p::parse-form (make-registry '()) input)))))
