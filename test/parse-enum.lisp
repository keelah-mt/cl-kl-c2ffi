(in-package :cl-kl-c2ffi/test/suite)
(in-suite parse-enum-forms)

(test should-parse-enum
  (let ((input '(enum test_enum (hello 0) (bye 1)))
        (expect (%p::form-def -1
                              (%p::fn-string "TEST_ENUM")
                              (%p::ft-enum '((:hello 0) (:bye 1))))))
    (is (equalp expect (%p:parse-form (make-registry '()) input)))))