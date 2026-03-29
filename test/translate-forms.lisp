(in-package :c2ffi/test/suite)
(in-suite translate-forms)

(test should-resolve-typedef-struct
  (let* ((inputs '((struct my-struct :size 64
                    (red :unsigned-short :bit-offset 0 :bit-size 64 :bit-alignment 64))
                   (typedef my-struct-t (:struct my-struct))))
         (context (run-parser inputs)))
    (is (equalp "1:$SYM:MY-STRUCT#Struct"
                (resolve-form "$SYM:MY-STRUCT-T#TypeDef" context)))))

(test should-not-resolve-typedef-empty-struct
  (let* ((inputs '((struct my-struct :size 0)
                   (typedef my-struct-t (:struct my-struct))))
         (context (run-parser inputs)))
    (is (equalp "1:$SYM:MY-STRUCT-T#TypeDef"
                (resolve-form "$SYM:MY-STRUCT-T#TypeDef" context)))))

(test should-use-form-rank-1
  (let* ((inputs '((struct my-struct :size 0)
                   (typedef my-struct-t (:struct my-struct))
                   (struct my-struct :size 64
                    (red :unsigned-short :bit-offset 0 :bit-size 64 :bit-alignment 64))))
         (context (run-parser inputs)))
    (is (equalp "1:$SYM:MY-STRUCT-T#TypeDef"
                (resolve-form "$SYM:MY-STRUCT-T#TypeDef" context)))))

(test should-use-form-rank-2
  (let* ((inputs '((struct my-struct :size 0)
                   (typedef my-struct-t (:struct my-struct))
                   (struct my-struct :size 64
                    (red :unsigned-short :bit-offset 0 :bit-size 64 :bit-alignment 64))
                   (typedef my-struct-t-1 (:struct my-struct))))
         (context (run-parser inputs)))
    (is (equalp "3:$SYM:MY-STRUCT#Struct"
                (resolve-form "$SYM:MY-STRUCT-T-1#TypeDef" context)))))

(test should-convert-basic-typedef
  (let* ((inputs '((typedef int8_t :signed-char)
                   (typedef int_least8_t int8_t)))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE INT_LEAST8_T INT8_T))"
                (convert-form "$SYM:INT_LEAST8_T#TypeDef" context)))))

(test should-convert-struct-ref-1
  (let* ((inputs '((struct resolve-me :size 64
                    (red :unsigned-short :bit-offset 0 :bit-size 64 :bit-alignment 64))
                   (typedef resolve-me (:struct resolve-me))
                   (struct tough-life :size 64
                    (success (:pointer resolve-me)
                     :bit-offset 0 :bit-size 64 :bit-alignemt 64))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCSTRUCT (TOUGH-LIFE :SIZE 8) (SUCCESS (:POINTER (:STRUCT RESOLVE-ME)) :OFFSET 0)))"
                (convert-form "$SYM:TOUGH-LIFE#Struct" context)))))

(test should-not-convert-empty-struct-ref-1
  (let* ((inputs '((struct do-not-resolve-me :size 0)
                   (typedef do-not-resolve-me (:struct do-not-resolve-me))
                   (struct tough-life :size 64
                    (success (:pointer do-not-resolve-me)
                     :bit-offset 0 :bit-size 64 :bit-alignemt 64))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCSTRUCT (TOUGH-LIFE :SIZE 8) (SUCCESS (:POINTER DO-NOT-RESOLVE-ME) :OFFSET 0)))"
                (convert-form "$SYM:TOUGH-LIFE#Struct" context)))))

;; TODO: is this really supported by CFFI? it probably ignores one :count
;; might need logic to replaces counts with a sum, so failing this test for now
(test should-convert-creative-x-struct-1
  (let* ((inputs '((struct transform :size 288
                    (matrix (:array (:array :int 3) 3)
                     :bit-offset 0 :bit-size 288 :bit-alignment 32))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCSTRUCT (TRANSFORM :SIZE 36) (MATRIX :INT :COUNT 9 :OFFSET 0)))"
                (convert-form "$SYM:TRANSFORM#Struct" context)))))

;; TODO: I bet there was no other way to define this struct...
;; currently at least quite annoying to fix if not outright hard, because the parser
;; was not designed for this.as if anything was designed in this project? :-D
(test should-convert-creative-x-struct-2
  (let* ((inputs '((typedef hints
                    (struct :id 133 :size 640
                     (min_aspect (struct :id 134 :size 64
                                   (x :int :bit-offset 0 :bit-size 32 :bit-alignment 32)
                                  (y :int :bit-offset 32 :bit-size 32 :bit-alignment 32))
                      :bit-offset 384 :bit-size 64 :bit-alignment 32)
                     (max_aspect (:struct :id 134)
                      :bit-offset 448 :bit-size 64 :bit-alignment 32)))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCSTRUCT (ID-134 :SIZE 8) (X :INT :OFFSET 0) (Y :INT :OFFSET 4)) (CFFI:DEFCSTRUCT (ID-134 :SIZE 8) (X :INT :OFFSET 0) (Y :INT :OFFSET 4)) (CFFI:DEFCSTRUCT (ID-133 :SIZE 80) (MIN_ASPECT (:STRUCT ID-134) :OFFSET 48) (MAX_ASPECT (:STRUCT ID-134) :OFFSET 56)) (CFFI:DEFCTYPE HINTS (:STRUCT ID-133)))"
                (convert-form "$SYM:HINTS#TypeDef" context)))))

(test should-convert-creative-x-function-1
  (let* ((inputs '((function "XQueryKeymap" (((:pointer :int)) ((:array :char 32))) :int)))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCFUN (\"XQueryKeymap\" |\"XQUERYKEYMAP\"|) :INT (ARG-0 (:POINTER :INT)) (ARG-1 (:POINTER :CHAR))))"
                (convert-form "$STR:\"XQueryKeymap\"#" context)))))

(test should-convert-function-1
  (let* ((inputs '((struct future :size 64
                    (ptr (:pointer :void) :bit-offset 0 :bit-size 64 :bit-alignemt 64))
                   (typedef future (:struct future))
                   (function "request" ((what :int)) future)))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCFUN (\"request\" |\"REQUEST\"|) (:STRUCT FUTURE) (WHAT :INT)))"
                (convert-form "$STR:\"request\"#" context)))))

(test should-convert-typedef-1
  (let* ((inputs '((typedef type1 :char)
                   (typedef type2 type1)))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE TYPE2 TYPE1))"
                (convert-form "$SYM:TYPE2#TypeDef" context)))))

(test should-convert-typedef-2
  (let* ((inputs '((typedef type1 :char)
                   (typedef type2 (:pointer type1))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE TYPE2 (:POINTER TYPE1)))"
                (convert-form "$SYM:TYPE2#TypeDef" context)))))

(test should-convert-typedef-3
  (let* ((inputs '((struct struct1 :size 64
                    (ptr (:pointer :void) :bit-offset 0 :bit-size 64 :bit-alignemt 64))
                   (typedef type2 (:struct struct1))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE TYPE2 (:STRUCT STRUCT1)))"
                (convert-form "$SYM:TYPE2#TypeDef" context)))))

(test should-convert-typedef-4
  (let* ((inputs '((struct struct1 :size 64
                    (ptr (:pointer :void) :bit-offset 0 :bit-size 64 :bit-alignemt 64))
                   (typedef type2 (:pointer (:struct struct1)))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE TYPE2 (:POINTER (:STRUCT STRUCT1))))"
                (convert-form "$SYM:TYPE2#TypeDef" context)))))

(test should-convert-typedef-5
  (let* ((inputs '((struct view :size 64
                    (data (:pointer :char) :bit-offset 0 :bit-size 64 :bit-alignemt 64))
                   (typedef view (:struct view))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE VIEW (:STRUCT VIEW)))"
                (convert-form "$SYM:VIEW#TypeDef" context)))))

(test should-convert-array-data-1
  (let* ((inputs '((struct my-struct :size 64
                    (val (:array :int 2) :bit-offset 0 :bit-size 64 :bit-alignment 32))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCSTRUCT (MY-STRUCT :SIZE 8) (VAL :INT :COUNT 2 :OFFSET 0)))"
                (convert-form "$SYM:MY-STRUCT#Struct" context)))))

(test should-convert-enum-1
  (let* ((inputs '((enum backend
                    (Undefined 0)
                    (Vulkan 1))))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCENUM BACKEND (:UNDEFINED 0) (:VULKAN 1)))"
                (convert-form "$SYM:BACKEND#Enum" context)))))

(test should-resolve-names-1
  (let* ((inputs '((typedef __fsid_t
                    (struct :id 2 :size 64
                     (__val (:array :int 2) :bit-offset 0 :bit-size 64 :bit-alignment 32)))
                   (typedef fsid_t __fsid_t)))
         (context (run-parser inputs)))
    (is (equalp "((CFFI:DEFCTYPE FSID_T (:STRUCT ID-2)))"
                (convert-form "$SYM:FSID_T#TypeDef" context)))))
