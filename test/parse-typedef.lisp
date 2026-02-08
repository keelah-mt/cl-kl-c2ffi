(in-package :c2ffi/test/suite)
(in-suite parse-typedef-forms)

(test should-parse-atom
  (let ((input '(typedef int_fast32_t :long))
        (expect (%p::form-def -1
                              (%p::fn-string "INT_FAST32_T")
                              (%p::ft-typedef-alias 0 :long 1))))
    (is (equalp expect (%p:parse-form (make-registry '()) input)))))

(test should-fail-parse-non-atom
  (let ((input '(typedef int_fast32_t :wong)))
    (signals type-error (%p:parse-form (make-registry '()) input))))

(test should-parse-pointed-atom
  (let ((input '(typedef simple_pointer_type (:pointer :void)))
        (expect (%p::form-def -1
                              (%p::fn-string "SIMPLE_POINTER_TYPE")
                              (%p::ft-typedef-alias 1 :void 1))))
    (is (equalp expect (%p:parse-form (make-registry '()) input)))))

(test should-fail-pointed-non-atom
  (let ((input '(typedef bad_pointer (:pointer :wong))))
    (signals simple-error (%p:parse-form (make-registry '()) input))))

(test should-parse-simple-alias
  (let ((input '(typedef simple_alias something_else))
        (expect (%p::form-def -1
                              (%p::fn-string "SIMPLE_ALIAS")
                              (%p::ft-typedef-alias 0 1 1)))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "SOMETHING_ELSE")
                                                (%p::ft-typedef-alias 0 :uchar 1))))))
    (is (equalp expect (%p:parse-form registry input)))))

;; TODO: cover missing alias

(test should-parse-pointed-alias
  (let ((input '(typedef pointed_alias (:pointer something_else)))
        (expect (%p::form-def -1
                              (%p::fn-string "POINTED_ALIAS")
                              (%p::ft-typedef-alias 1 1 1)))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "SOMETHING_ELSE")
                                                (%p::ft-typedef-alias 0 :uchar 1))))))
    (is (equalp expect (%p:parse-form registry input)))))

(test should-fail-parse-pointed-to-missing-alias
  (let ((input '(typedef missing_aliased_type (:pointer doesnt_exist))))
    (signals type-error (%p:parse-form (make-registry '()) input))))

(test should-parse-struct-alias
  (let ((input '(typedef typedef_struct (:struct typedef_struct)))
        (expect (%p::form-def -1
                              (%p::fn-string "TYPEDEF_STRUCT")
                              (%p::ft-typedef-alias 0 1 1)))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "TYPEDEF_STRUCT")
                                                (%p::ft-struct 0 '()))))))
    (is (equalp expect (%p:parse-form registry input)))))

(test should-parse-pointed-struct-alias
  (let ((input '(typedef typedef_pointed_struct (:pointer (:struct some_struct))))
        (expect (%p::form-def -1
                              (%p::fn-string "TYPEDEF_POINTED_STRUCT")
                              (%p::ft-typedef-alias 1 1 1)))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "SOME_STRUCT")
                                                (%p::ft-struct 0 '()))))))
    (is (equalp expect (%p:parse-form registry input)))))

(test should-parse-deeply-pointed-atom
  (let ((input '(typedef nested_pointer (:pointer (:pointer (:pointer :void)))))
        (expect (%p:form-def -1
                             (%p:fn-string "NESTED_POINTER")
                             (%p:ft-typedef-alias 3 :void 1))))
    (is (equalp expect (%p:parse-form (make-registry '()) input)))))

(test should-parse-deeply-pointer-alias
  (let ((input '(typedef nested_pointer (:pointer (:pointer other_pointed))))
        (expect (%p:form-def -1
                             (%p:fn-string "NESTED_POINTER")
                             (%p:ft-typedef-alias 2 2 1)))
        (registry (make-registry (list
                                  (%p:form-def -1
                                               (%p:fn-string "atom")
                                               (%p:ft-typedef-alias 0 :int 1))
                                  (%p:form-def -1
                                               (%p:fn-string "OTHER_POINTED")
                                               (%p:ft-typedef-alias 1 1 1))))))
    (is (equalp expect (%p:parse-form registry input)))))

(test should-fail-parse-missing-struct-alias
  (let ((input '(typedef typedef_struct (:struct missing_struct))))
    (signals type-error (%p:parse-form (make-registry '()) input))))

;; TODO: (struct :what)

(test should-fail-parse-non-struct-alias
  (let ((input '(typedef typedef_struct (:struct not_struct)))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "NOT_STRUCT")
                                                (%p::ft-typedef-alias 0 :char 1))))))
    (is (eq 1 (%p:registry-lookup-form-id registry
                                          '%p::ft-typedef-alias (%p::fn-string "NOT_STRUCT"))))
    (is (null (%p:registry-lookup-form-id registry
                                          '%p::ft-struct (%p::fn-string "NOT_STRUCT"))))
    (signals type-error (%p:parse-form registry input))))

(test should-parse-enum-alias
  (let ((input '(typedef typedef_enum (:enum test_enum)))
        (expect (%p::form-def -1
                              (%p::fn-string "TYPEDEF_ENUM")
                              (%p::ft-typedef-alias 0 1 1)))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "TEST_ENUM")
                                                (%p::ft-enum '((:option1 0))))))))
    (is (equalp expect (%p:parse-form registry input)))))
