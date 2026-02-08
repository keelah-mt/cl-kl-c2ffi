(in-package :c2ffi/test/suite)
(in-suite parse-struct-forms)

;; (test parse-struct-empty
;; (let ((input '(struct empty_struct :size 0))
;; (expect (%p::form-def (%p::fn-string "EMPTY_STRUCT") ())))))

(test parse-struct
  (let ((input
          '(struct some_struct :size 224
            (str_f (:pointer :char) :bit-offset 0 :bit-size 64 :bit-alignment 64)
            (int_f :int :bit-offset 64 :bit-size 32 :bit-alignment 32)
            (ptr_alias_f (:pointer alias_1) :bit-offset 96 :bit-size 64 :bit-alignment 64)
            (val_alias_f alias_2 :bit-offset 160 :bit-size 32 :bit-alignment 32)
            (no_td_struct (:struct struct_no_td) :bit-offset 192 :bit-size 32 :bit-alignment 32)))
        (expect (%p::form-def
                 5
                 (%p::fn-string "SOME_STRUCT")
                 (%p::ft-struct 28 (list
                                     (list (%p::form-def -1
                                                         (%p::fn-string "STR_F")
                                                         (%p::ft-typedef-alias 1 :char 1))
                                           '(:offset 0 :size 8 :alignment 8))
                                     (list (%p::form-def -1
                                                         (%p::fn-string "INT_F")
                                                         (%p::ft-typedef-alias 0 :int 1))
                                           '(:offset 8 :size 4 :alignment 4))
                                     (list (%p::form-def -1
                                                         (%p::fn-string "PTR_ALIAS_F")
                                                         (%p::ft-typedef-alias 1 1 1))
                                           '(:offset 12 :size 8 :alignment 8))
                                     (list (%p::form-def -1
                                                         (%p::fn-string "VAL_ALIAS_F")
                                                         (%p::ft-typedef-alias 0 3 1))
                                           '(:offset 20 :size 4 :alignment 4))
                                     (list (%p::form-def -1
                                                         (%p::fn-string "NO_TD_STRUCT")
                                                         (%p::ft-typedef-alias 0 4 1))
                                           '(:offset 24 :size 4 :alignment 4))))))
        (registry (make-registry (list
                                  (%p::form-def -1
                                                (%p::fn-string "ALIAS_1")
                                                (%p::ft-typedef-alias 1 :char 1))
                                  (%p::form-def -1
                                                (%p::fn-string "SOME_FLOAT")
                                                (%p::ft-typedef-alias 0 :float 1))
                                  (%p::form-def -1
                                                (%p::fn-string "ALIAS_2")
                                                (%p::ft-typedef-alias 0 2 1))
                                  (%p::form-def -1
                                                (%p::fn-string "STRUCT_NO_TD")
                                                (%p::ft-struct 0 '()))))))
    (is (equalp expect (%p:parse-form registry input)))))