(in-package :cl-kl-c2ffi/test/suite)
(in-suite parse-function-forms)

(test should-parse-function-form
  (let ((input '(function "some_function"
                 ((arg1 some_type) (arg2 (:pointer :int)))
                 some_return_type))
        (expect (%p::form-def -1
                              (%p::fn-string "SOME_FUNCTION")
                              (%p::ft-function (%p::form-def -1
                                                             (%p::fn-string "RETURN-TYPE")
                                                             (%p::ft-typedef-alias 0 1 1))
                                               (list
                                                (%p::form-def -1
                                                              (%p::fn-string "ARG1")
                                                              (%p::ft-typedef-alias 0 2 1))
                                                (%p::form-def -1
                                                              (%p::fn-string "ARG2")
                                                              (%p::ft-typedef-alias 1 :int 1))))))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "SOME_RETURN_TYPE")
                                                (%p::ft-typedef-alias 1 :char 1))
                                  (%p::form-def -1
                                                (%p::fn-string "SOME_TYPE")
                                                (%p::ft-typedef-alias 0 :int 1))))))
    (is (equalp expect (%p:parse-form registry input)))))
