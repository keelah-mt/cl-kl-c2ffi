(in-package :cl-kl-c2ffi/test/suite)
(in-suite process-input)

(test can-translate-typedef-atom
  (let ((registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "simple")
                                                (%p:ft-typedef-alias 0 :int 1))
                                  (%p::form-def -1
                                                (%p::fn-string "pointed")
                                                (%p:ft-typedef-alias 1 :char 1)))))
        (expect '((cffi:defctype simple :int)
                  (cffi:defctype pointed (:pointer :char)))))
    (is (equalp expect (%t:translate registry)))))

(test can-translate-typedef-nested-pointed-atom
  (let ((registry (make-registry (list
                                  (%p:form-def -1
                                               (%p:fn-string "atom")
                                               (%p:ft-typedef-alias 2 :int 1)))))
        (expect '((cffi:defctype atom (:pointer (:pointer :int))))))
    (is (equalp expect (%t:translate registry)))))

(test can-translate-typedef-alias
  (let ((registry (make-registry (list
                                  (%p:form-def -1
                                               (%p:fn-string "atom")
                                               (%p:ft-typedef-alias 0 :int 1))
                                  (%p::form-def -1
                                                (%p:fn-string "alias")
                                                (%p:ft-typedef-alias 0 1 1))))))
    (is (equalp '(cffi:defctype alias atom) (second (%t:translate registry))))))

(test can-translate-typedef-pointed-alias
  (let ((registry (make-registry (list
                                  (%p:form-def -1
                                               (%p:fn-string "string")
                                               (%p:ft-typedef-alias 1 :char 1))
                                  (%p:form-def -1
                                               (%p:fn-string "my_strings")
                                               (%p:ft-typedef-alias 1 1 1)))))
        (expect '(cffi:defctype MY-STRINGS (:pointer STRING))))
    (is (equalp expect (second (%t:translate registry :resolve-aliases nil))))))

(test can-translate-typedef-alias-full-resolve
  (let ((registry (make-registry (list
                                  (%p:form-def -1
                                               (%p:fn-string "atom")
                                               (%p:ft-typedef-alias 0 :int 1))
                                  (%p::form-def -1
                                                (%p:fn-string "alias")
                                                (%p:ft-typedef-alias 0 1 1))))))
    (is (equalp '(cffi:defctype alias :int) (second (%t:translate registry :resolve-aliases t))))))

(test can-translate-typedef-pointed-alias-full-resolve
  (let ((registry (make-registry (list
                                  (%p:form-def -1
                                               (%p:fn-string "string")
                                               (%p:ft-typedef-alias 1 :char 1))
                                  (%p:form-def -1
                                               (%p:fn-string "my_strings")
                                               (%p:ft-typedef-alias 1 1 1)))))
        (expect '(cffi:defctype MY-STRINGS (:pointer (:pointer :char)))))
    (is (equalp expect (second (%t:translate registry :resolve-aliases t))))))


(test can-translate-const-form
  (let ((registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "test_const")
                                                (%p::ft-const :int 99))))))
    (is (equalp '((alexandria:define-constant TEST-CONST 99)) (%t:translate registry)))))

(test can-translate-enum-form
  (let ((registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "test_enum")
                                                (%p::ft-enum '((:hello 1) (:bye 0))))))))
    (is (equalp '((cffi:defcenum ((:hello 1) (:bye 0)))) (%t:translate registry)))))

(test can-translate-struct-form
  (let ((registry (make-registry
                   (list
                    (%p::form-def -1
                                  (%p::fn-string "struct_type")
                                  (%p::ft-struct 4
                                                 (list
                                                  (list
                                                   (%p:form-def -1
                                                                (%p:fn-string "int_f")
                                                                (%p:ft-typedef-alias 0 :uint32 1))
                                                   '(:offset 0 :size 4 :alignment 8)))))
                    (%p:form-def -1
                                 (%p:fn-string "struct_type_alias")
                                 (%p:ft-typedef-alias 0 1 1))
                    (%p::form-def -1
                                  (%p::fn-string "opaque_struct_type")
                                  (%p::ft-struct 0 '()))
                    (%p:form-def -1
                                 (%p:fn-string "opaque_struct_ptr_alias")
                                 (%p:ft-typedef-alias 1 3 1))
                    (%p::form-def -1
                                  (%p::fn-string "test_struct")
                                  (%p::ft-struct
                                   24
                                   (list
                                    (list (%p:form-def -1
                                                       (%p:fn-string "size_field")
                                                       (%p:ft-typedef-alias 0 :size 1))
                                          '(:offset 0 :size 8 :alignment 8))
                                    (list (%p:form-def -1
                                                       (%p:fn-string "struct_field")
                                                       (%p:ft-typedef-alias 0 1 1))
                                          '(:offset 8 :size 8 :alignment 8))
                                    (list (%p:form-def -1
                                                       (%p:fn-string "struct_field_alias")
                                                       (%p:ft-typedef-alias 0 2 1))
                                          '(:offset 16 :size 8 :alignment 8))
                                    (list (%p:form-def -1
                                                       (%p:fn-string "pointer_struct_field")
                                                       (%p:ft-typedef-alias 1 1 1))
                                          '(:offset 24 :size 8 :alignment 8))
                                    (list (%p:form-def -1
                                                       (%p:fn-string "opaque_field")
                                                       (%p:ft-typedef-alias 1 3 1))
                                          '(:offset 32 :size 8 :alignment 8))
                                    (list (%p:form-def -1
                                                       (%p:fn-string "opaque_field_alias_ptr")
                                                       (%p:ft-typedef-alias 1 4 1))
                                          '(:offset 40 :size 8 :alignment 8)))))))))
    (is (equalp '(cffi:defcstruct (test-struct :size 24)
                  (size-field :size :offset 0)
                  (struct-field (:struct struct-type) :offset 8)
                  (struct-field-alias (:struct struct-type) :offset 16)
                  (pointer-struct-field (:pointer (:struct struct-type)) :offset 24)
                  (opaque-field (:pointer (:struct opaque-struct-type)) :offset 32)
                  (opaque-field-alias-ptr (:pointer opaque-struct-ptr-alias) :offset 40))
                (fifth (%t:translate registry))))))