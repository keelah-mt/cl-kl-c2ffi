(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare parse-ctype (Symbol -> (Optional CType)))
  (define (parse-ctype sym)
    (match (%str:downcase (%sym:symbol-name sym))
      ;; --- Native C Types (Require Signedness) ---
      ("char"                (Some (CTNative CNChar Signed)))
      ("signed-char"         (Some (CTNative CNChar Signed)))
      ("unsigned-char"       (Some (CTNative CNChar Unsigned)))
      ("uchar"               (Some (CTNative CNChar Unsigned)))

      ("short"               (Some (CTNative CNShort Signed)))
      ("unsigned-short"      (Some (CTNative CNShort Unsigned)))
      ("ushort"              (Some (CTNative CNShort Unsigned)))

      ("int"                 (Some (CTNative CNInt Signed)))
      ("unsigned-int"        (Some (CTNative CNInt Unsigned)))
      ("uint"                (Some (CTNative CNInt Unsigned)))

      ("long"                (Some (CTNative CNLong Signed)))
      ("unsigned-long"       (Some (CTNative CNLong Unsigned)))
      ("ulong"               (Some (CTNative CNLong Unsigned)))

      ("long-long"           (Some (CTNative CNLongLong Signed)))
      ("unsigned-long-long"  (Some (CTNative CNLongLong Unsigned)))
      ("llong"               (Some (CTNative CNLongLong Signed)))
      ("ullong"              (Some (CTNative CNLongLong Unsigned)))

      ;; --- Fixed Width / System Types (Self-Signed) ---
      ("int8"                (Some (CTSystem CSI8)))
      ("uint8"               (Some (CTSystem CSU8)))
      ("int16"               (Some (CTSystem CSI16)))
      ("uint16"              (Some (CTSystem CSU16)))
      ("int32"               (Some (CTSystem CSI32)))
      ("uint32"              (Some (CTSystem CSU32)))
      ("int64"               (Some (CTSystem CSI64)))
      ("uint64"              (Some (CTSystem CSU64)))

      ("size"                (Some (CTSystem CSSize)))
      ("ssize"               (Some (CTSystem CSSSize)))
      ("intptr"              (Some (CTSystem CSIntPtr)))
      ("uintptr"             (Some (CTSystem CSUIntPtr)))
      ("ptrdiff"             (Some (CTSystem CSPtrDiff)))
      ("offset"              (Some (CTSystem CSOffset)))

      ;; --- Floating Point ---
      ("float"               (Some (CTFloat FPSingle)))
      ("double"              (Some (CTFloat FPDouble)))
      ("long-double"         (Some (CTFloat FPLongDouble)))
      ("__float128"          (Some (CTFloat FPLongDouble)))

      ;; --- Special ---
      ("void"                (Some CTVoid))
      ("function-pointer"    (Some CTFunctionPointer))

      (_ None)))

  (declare validate ((:a -> (Optional String)) -> Parser :a -> Parser :a))
  (define (validate f p)
    (let ((pf (get-parser p))
          (pn (mconcat (make-list "(validate:" (get-parser-name p) ")"))))
      (Parser
       pn
       (fn (input state)
         (match (pf input state)
           ((Ok (Tuple3 v c i))
            (match (f v)
              ((None)
               (Ok (Tuple3 v c i)))
              ((Some str)
               (Err (push-error input
                                pn
                                (<> "expected: " str)
                                (make-error-stack))))))
           ((Err e) (Err (push-trace pn "validate failed in reader" e))))))))

  (declare read-symbol (Parser Symbol))
  (define read-symbol
    (let ((pn "read-symbol"))
      (Parser
       pn
       (fn (input state)
         (bimap (fn (e) (push-error input pn e (make-error-stack)))
                (fn (result) (Tuple3 (fst result) state (snd result)))
                (%take-symbol input))))))

  (declare read-keyword (Parser Symbol))
  (define read-keyword
    (validate (fn (v)
                (if (%sym:keyword? v)
                    None
                    (Some "keyword")))
              (Parser "read-keyword" (get-parser read-symbol))))

  (declare read-ctype (Parser CType))
  (define read-ctype
    (let ((pf (get-parser read-keyword))
          (pn "read-ctype"))
      (Parser
       pn
       (fn (input state)
         (match (pf input state)
           ((Ok (Tuple3 v c i))
            (match (parse-ctype v)
              ((Some ctype)
               (Ok (Tuple3 ctype c i)))
              ((None)
               (Err (push-error input
                                pn
                                (<> "expected c-type, got: :" (%sym:symbol-name v))
                                (make-error-stack))))))
           ((Err e) (Err (push-trace pn "read-ctype failed in keyword reader" e))))))))

  (declare read-string (Parser String))
  (define read-string
    (let ((pn "read-string"))
      (Parser
       pn
       (fn (input state)
         (bimap (fn (e) (push-error input pn e (make-error-stack)))
                (fn (result) (Tuple3 (fst result) state (snd result)))
                (%take-string input))))))

  (declare read-integer (Parser Integer))
  (define read-integer
    (let ((pn "read-integer"))
      (Parser
       pn
       (fn (input state)
         (bimap (fn (e) (push-error input pn e (make-error-stack)))
                (fn (result) (Tuple3 (fst result) state (snd result)))
                (%take-integer input))))))

  (define (string-icase? str)
    (let ((dcase (%str:downcase str)))
      (validate (fn (v) (if (== v dcase) None (Some dcase)))
                (map %str:downcase read-string))))

  (declare keyword? (Symbol -> (Parser Symbol)))
  (define (keyword? kw)
    (validate (fn (v)
                (let ((v-str (%str:downcase (%sym:symbol-name v)))
                      (kw-str (%str:downcase (%sym:symbol-name kw))))
                  (if (== v-str kw-str)
                      None
                      (Some (<> "keyword: :" (%sym:symbol-name kw))))))
              (Parser (<> "keyword?" (%sym:symbol-name kw)) (get-parser read-keyword))))

  (declare open-list ((Parser :a) -> (Parser :a)))
  (define (open-list p)
    (let ((pf (get-parser p))
          ;;(pn (mconcat (make-list "open-list(" (get-parser-name p) ")")))
          (pn "open-list"))
      (Parser
       pn
       (fn (input state)
         (match (%take-list input)
           ((Ok (Tuple list tail))
            (match (pf list state)
              ;; TODO: check list is consumed?
              ;; TODO: which context should be passed here?
              ((Ok (Tuple3 v c _list-eof))
               (Ok (Tuple3 v c tail)))
              ((Err e) (Err e))))
           ((Err e) (Err (push-error input pn e (make-error-stack)))))))))

  (declare empty-list ((Parser :a) -> (Parser (List :a))))
  (define (empty-list _p)
    (Parser
     "empty-list"
     (fn (input state)
       (match (%empty-list input)
         ((Ok (Tuple _list tail))
          (Ok (Tuple3 (make-list) state tail)))
         ((Err e)
          (Err (push-error input "empty-list" e (make-error-stack))))))))

  (declare many ((Integer -> (Parser :a)) -> (Parser (List :a))))
  (define (many p)
    (let ((iter (fn (input context counter)
                  (let ((pfi (get-parser (p counter))))
                    (match (pfi input context)
                      ((Err _) (Tuple3 Nil context input))
                      ((Ok (Tuple3 va ca ia))
                       (match (iter ia ca (1+ counter))
                         ((Tuple3 vb cb ib) (Tuple3 (Cons va vb) cb ib)))))))))
      (Parser
       (<> "many:" (get-parser-name (p 0)))
       (fn (input context)
         (Ok (iter input context 0))))))

)
