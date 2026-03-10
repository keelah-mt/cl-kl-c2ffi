(in-package :c2ffi/test/suite)
(in-suite parse-typedef-forms)

(test should-parse-id-name
  (let ((input '(:id 1)))
    (is (equalp "[ID]1" (run-parser-wrap input %p::form-name-id)))))

(test should-parse-atom
  (let ((input '(typedef int_fast32_t :long)))
    (is (equalp "[SYM]INT_FAST32_T->#KTypeDef:[KW]:LONG->#KAtom"
                (run-parser-wrap input %p:typedef)))))

(test should-parse-scalar-alias
  (let* ((input '((typedef __int8_t :long)
                 (typedef __int_least8_t __int8_t)))
         (expect "[SYM]__INT_LEAST8_T->#KTypeDef:[SYM]__INT8_T->#KTypeDef:[KW]:LONG->#KAtom")
         (result (%p:get-parse-result (%p:parse input))))
    (is (equalp expect (%p:find-result "[SYM]__INT_LEAST8_T" result)))))

(test should-parse-compound-alias-simple
  (let* ((input '(typedef target (:pointer :long)))
         (expect ))
    (is (equalp "[SYM]TARGET->#KTypeDef:[STR]TODO-GEN-ID->#KCompound(P:1,[KW]:LONG->#KAtom,C:0)"
                (run-parser-wrap input %p:typedef)))))

(test should-parse-compound-alias-lookup
  (let* ((input '((typedef my_long :long)
                  (typedef target (:pointer my_long))))
         (expect "[SYM]TARGET->#KTypeDef:[STR]TODO-GEN-ID->#KCompound(P:1,[SYM]MY_LONG->#KTypeDef:[KW]:LONG->#KAtom,C:0)")
         (result (%p:get-parse-result (%p:parse input))))
    (is (equalp expect (%p:find-result "[SYM]TARGET" result)))))

(test should-parse-form-param
  (let ((input '(:bit-offset 12)))
    (is (equalp "|OFFSET:12|"
                (run-parser-wrap input %p::form-param-any)))))

(test should-parse-atom-array-compound
  (let ((input '(:array :unsigned-char 15)))
    (is (equalp "(P:0,[KW]:UNSIGNED-CHAR->#KAtom,C:15)"
                (run-parser-wrap input %p::array-compound)))))

(test should-parse-atom-pointer-compound
  (let ((input '(:pointer :unsigned-char)))
    (is (equalp "(P:1,[KW]:UNSIGNED-CHAR->#KAtom,C:0)"
                (run-parser-wrap input %p::pointer-compound)))))

(test should-parse-simple-struct-field
  (let ((input '(allocate (:pointer :void) :bit-offset 0 :bit-size 64 :bit-alignment 64)))
    (is (equalp "FIELD:[SYM]ALLOCATE[STR]TODO-GEN-ID->#KCompound(P:1,[KW]:VOID->#KAtom,C:0)|OFFSET:0||SIZE:64||ALIGN:64|"
                (run-parser-wrap input %p::struct-field)))))

(test should-parse-simple-struct
  (let ((input '(struct GLFWvidmode :size 64
                 (width :int :bit-offset 0 :bit-size 32 :bit-alignment 32)
                 (height :int :bit-offset 32 :bit-size 32 :bit-alignment 32))))
    (is (equalp "[SYM]GLFWVIDMODE->#KStruct:|SIZE:64|FIELD:[SYM]WIDTH[KW]:INT->#KAtom|OFFSET:0||SIZE:32||ALIGN:32|FIELD:[SYM]HEIGHT[KW]:INT->#KAtom|OFFSET:32||SIZE:32||ALIGN:32|" (run-parser-wrap input %p::struct)))))

(test should-parse-simple-struct-id-name
  (let ((input '(struct :id 1 :size 64
                 (width :int :bit-offset 0 :bit-size 32 :bit-alignment 32))))
    (is (equalp "[ID]1->#KStruct:|SIZE:64|FIELD:[SYM]WIDTH[KW]:INT->#KAtom|OFFSET:0||SIZE:32||ALIGN:32|" (run-parser-wrap input %p::struct)))))

(test should-parse-embedded-struct-typedef
  (let* ((input '((typedef max_align_t
                   (struct :id 1 :size 64
                    (nonce1 :long-long :bit-offset 0 :bit-size 64 :bit-alignment 64)))))
         (result (%p:get-parse-result (%p:parse input))))
    (is (equalp "" (%p:find-result "[SYM]MAX_ALIGN_T" result)))))
