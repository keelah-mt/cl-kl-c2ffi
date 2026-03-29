(in-package :c2ffi/test/suite)
(in-suite parse-forms)

;; TODO: add tests for failures/not consuming input

;; -------------------- BASIC ATOM/COMPOUND VALUES --------------------

(test should-parse-atom
  (let ((inputs '((typedef int_fast32_t :long))))
    (is (equalp "0:$SYM:INT_FAST32_T->#KTypeDef:$NONE:->#KAtom:#.(%P::CTNATIVE #.%P::CNLONG #.%P::SIGNED)"
                (find-result "$SYM:INT_FAST32_T#TypeDef" (run-parser inputs))))))

(test should-lookup-form
  (let ((inputs '((typedef look-at-me :char)
                  (typedef look-at-you look-at-me))))
    (is (equalp "1:$SYM:LOOK-AT-YOU->#KTypeDef:$NONE:->#KLookup:$SYM:LOOK-AT-ME#TypeDef"
                (find-result "$SYM:LOOK-AT-YOU#TypeDef" (run-parser inputs))))))

(test should-parse-compound-pointer-1
  (let ((inputs '((typedef cmp1 (:pointer :void)))))
    (is (equalp "0:$SYM:CMP1->#KTypeDef:$NONE:->#KCompound:@P:@T:[$NONE:->#KAtom:#.%P::CTVOID]"
                (find-result "$SYM:CMP1#TypeDef" (run-parser inputs))))))

(test should-parse-compound-pointer-2
  (let ((inputs '((typedef cmp1 (:pointer (:pointer :char))))))
    (is (equalp "0:$SYM:CMP1->#KTypeDef:$NONE:->#KCompound:@P:@P:@T:[$NONE:->#KAtom:#.(%P::CTNATIVE #.%P::CNCHAR #.%P::SIGNED)]"
                (find-result "$SYM:CMP1#TypeDef" (run-parser inputs))))))

(test should-parse-array-1
  (let ((inputs '((typedef arr (:array :char 32)))))
    (is (equalp "0:$SYM:ARR->#KTypeDef:$NONE:->#KCompound:@A[32]:@T:[$NONE:->#KAtom:#.(%P::CTNATIVE #.%P::CNCHAR #.%P::SIGNED)]"
                (find-result "$SYM:ARR#TypeDef" (run-parser inputs))))))

(test should-parse-array-2
  (let ((inputs '((typedef arr (:array (:array :char 10) 20)))))
    (is (equalp "0:$SYM:ARR->#KTypeDef:$NONE:->#KCompound:@A[20]:@A[10]:@T:[$NONE:->#KAtom:#.(%P::CTNATIVE #.%P::CNCHAR #.%P::SIGNED)]"
                (find-result "$SYM:ARR#TypeDef" (run-parser inputs))))))


;; -------------------- ENUMS --------------------

(test can-parse-enum-1
  (let ((inputs '((enum WGPUSType
                   (WGPUSType_ShaderSourceSPIRV 1)
                   (WGPUSType_ShaderSourceWGSL 2)))))
    (is (equalp "0:$SYM:WGPUSTYPE->#KEnum:@F:[$SYM:WGPUSTYPE_SHADERSOURCESPIRV:1|$SYM:WGPUSTYPE_SHADERSOURCEWGSL:2]"
                (find-result "$SYM:WGPUSTYPE#Enum" (run-parser inputs))))))

;; -------------------- STRUCTS --------------------

(test should-parse-struct-1
  (let ((inputs '((struct my-struct :size 64
                   (red :unsigned-short :bit-offset 0 :bit-size 64 :bit-alignment 64)))))
    (is (equalp "0:$SYM:MY-STRUCT->#KStruct:[SIZE:64]@F:[$SYM:RED->#KAtom:#.(%P::CTNATIVE #.%P::CNSHORT #.%P::UNSIGNED)<OFFSET:0;SIZE:64;ALIGN:64>]"
                (find-result "$SYM:MY-STRUCT#Struct" (run-parser inputs))))))

(test should-parse-struct-2
  (let ((inputs '((typedef no-look-at-me (:array :char 10))
                  (typedef look-at-me no-look-at-me)
                  (struct my-struct :size 999
                   (field1 no-look-at-me :bit-offset 0 :bit-size 32 :bit-alignment 32)
                   (field2 (:pointer look-at-me) :bit-offset 32 :bit-size 32 :bit-alignment 32)))))
    (is (equalp "2:$SYM:MY-STRUCT->#KStruct:[SIZE:999]@F:[$SYM:FIELD1->#KLookup:$SYM:NO-LOOK-AT-ME#TypeDef<OFFSET:0;SIZE:32;ALIGN:32>|$SYM:FIELD2->#KCompound:@P:@T:[$NONE:->#KLookup:$SYM:LOOK-AT-ME#TypeDef]<OFFSET:32;SIZE:32;ALIGN:32>]"
                (find-result "$SYM:MY-STRUCT#Struct" (run-parser inputs))))))

(test should-parse-struct-typedef-1
  (let ((inputs '((struct my-struct :size 0)
                  (typedef your-struct (:struct my-struct)))))
    (is (equalp "1:$SYM:YOUR-STRUCT->#KTypeDef:$NONE:->#KCompound:@T:[$NONE:->#KLookup:$SYM:MY-STRUCT#Struct]"
                (find-result "$SYM:YOUR-STRUCT#TypeDef" (run-parser inputs))))))

(test should-parse-embedded-struct-typedef-1
  (let ((inputs '((typedef max_align_t
                   (struct :id 1 :size 128
                    (nonce1 :long-long :bit-offset 0 :bit-size 64 :bit-alignment 64))))))
    (is (equalp "0:$SYM:MAX_ALIGN_T->#KTypeDef:$ID:1->#KInlined:$ID:1->#KStruct:[SIZE:128]@F:[$SYM:NONCE1->#KAtom:#.(%P::CTNATIVE #.%P::CNLONGLONG #.%P::SIGNED)<OFFSET:0;SIZE:64;ALIGN:64>]"
                (find-result "$SYM:MAX_ALIGN_T#TypeDef" (run-parser inputs))))))

(test can-resolve-names-1
  (let* ((inputs '((struct my-shared-name :size 0)
                   (typedef my-shared-name (:struct my-shared-name))))
         (result (run-parser inputs)))
    (is (equalp "0:$SYM:MY-SHARED-NAME->#KStruct:[SIZE:0]@F:[]"
                (find-result "$SYM:MY-SHARED-NAME#Struct" result)))
    (is (equalp "1:$SYM:MY-SHARED-NAME->#KTypeDef:$NONE:->#KCompound:@T:[$NONE:->#KLookup:$SYM:MY-SHARED-NAME#Struct]"
                (find-result "$SYM:MY-SHARED-NAME#TypeDef" result)))))

;; TODO: no it doesn't :-)

(test can-parse-struct-recursion-1
  (let ((inputs '((struct _XExtData :size 256
                   (number :int :bit-offset 0 :bit-size 32 :bit-alignment 32)
                   (next (:pointer (:struct _XExtData)) :bit-offset 64 :bit-size 64 :bit-alignment 64)))))
    (is (equalp "0:$SYM:_XEXTDATA->#KStruct:[SIZE:256]@F:[$SYM:NUMBER->#KAtom:#.(%P::CTNATIVE #.%P::CNINT #.%P::SIGNED)<OFFSET:0;SIZE:32;ALIGN:32>|$SYM:NEXT->#KCompound:@P:@T:[$NONE:->#KAtom:#.%P::CTVOID]<OFFSET:64;SIZE:64;ALIGN:64>]"
                (find-result "$SYM:_XEXTDATA#Struct" (run-parser inputs))))))

(test can-parse-struct-recursion-2
  (let ((inputs '((enum WGPUSType
                   (WGPUSType_ShaderSourceSPIRV 1)
                   (WGPUSType_ShaderSourceWGSL 2))
                  (typedef WGPUSType (:enum WGPUSType))
                  (struct WGPUChainedStructOut :size 128
                   (next (:pointer (:struct WGPUChainedStructOut))
                    :bit-offset 0 :bit-size 64 :bit-alignment 64)
                   (sType WGPUSType :bit-offset 64 :bit-size 32 :bit-alignment 32)))))
    (is (equalp "2:$SYM:WGPUCHAINEDSTRUCTOUT->#KStruct:[SIZE:128]@F:[$SYM:NEXT->#KCompound:@P:@T:[$NONE:->#KAtom:#.%P::CTVOID]<OFFSET:0;SIZE:64;ALIGN:64>|$SYM:STYPE->#KLookup:$SYM:WGPUSTYPE#TypeDef<OFFSET:64;SIZE:32;ALIGN:32>]"
                (find-result "$SYM:WGPUCHAINEDSTRUCTOUT#Struct" (run-parser inputs))))))

;; -------------------- FUNCTIONS --------------------

(test should-parse-fn-1
  (let ((inputs '((struct wl_display :size 0)
                  (function "my-fn" nil (:pointer (struct wl_display :size 0))))))
    (is (equalp "1:$STR:\"my-fn\"->#KFunction:[@RET<#KCompound:@P:@T:[$SYM:WL_DISPLAY->#KInlined:$SYM:WL_DISPLAY->#KStruct:[SIZE:0]@F:[]]>@ARG<>]"
                (find-result "$STR:\"my-fn\"#" (run-parser inputs))))))

(test should-parse-fn-2
  (let ((inputs '((typedef Arg :int)
                  (function "stuff" ((Arg) (Arg)) :void))))
    (is (equalp "1:$STR:\"stuff\"->#KFunction:[@RET<#KAtom:#.%P::CTVOID>@ARG<$SYM:#:|arg-0|->#KLookup:$SYM:ARG#TypeDef|$SYM:#:|arg-1|->#KLookup:$SYM:ARG#TypeDef>]"
                (find-result "$STR:\"stuff\"#" (run-parser inputs))))))

;; -------------------- CONST VALUES --------------------

(test should-parse-const-1
      (let ((inputs '((typedef WGPUFlags :int)
                      (typedef WGPUBufferUsage WGPUFlags)
                      (const WGPUBufferUsage_None WGPUBufferUsage 0))))
        (is (equalp "2:$SYM:WGPUBUFFERUSAGE_NONE->#KConst:[@TYPE<$NONE:->#KLookup:$SYM:WGPUBUFFERUSAGE#TypeDef>0]"
                    (find-result "$SYM:WGPUBUFFERUSAGE_NONE#" (run-parser inputs))))))
