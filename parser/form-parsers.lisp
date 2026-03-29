(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;; -------------------- FORM-NAME --------------------

  (declare form-name-sym (Parser FormName))
  (define form-name-sym
    (map FNSymbol read-symbol))

  (declare form-name-kw (Parser FormName))
  (define form-name-kw
    (map FNKeyword read-keyword))

  (declare form-name-string (Parser FormName))
  (define form-name-string
    (map FNString read-string))

  (declare form-name-id (Parser FormName))
  (define form-name-id
    (liftA2 (fn (_ id) (FNId id))
            (keyword? (%sym:make-keyword "id")) read-integer))

  ;; --------------------  UTILS --------------------

  ;; TODO: C can be very creative..
  ;; this would require registry update threaded everywhere
  ;; (or some other local lookups mechanism) and I just can't be bothered atm
  ;;(struct :id 133
  ;;  (min_aspect
  ;;    (struct :id 134 :size 64
  ;;     (x :int :bit-offset 0 :bit-size 32 :bit-alignment 32)
  ;;     (y :int :bit-offset 32 :bit-size 32 :bit-alignment 32)))
  ;;  (max_aspect (:struct :id 134) :bit-offset 448 :bit-size 64 :bit-alignment 32)

  (declare lookup (LookupTag -> (Parser Form)))
  (define (lookup tag)
    (let ((pf (get-parser (alt form-name-id form-name-sym)))
          (pn "lookup")
          ;; if this is a struct and lookup is failing, chances are it is self-pointer
          ;; so just set it to :pointer :void because cffi doesn't care about its type atm anyway
          (should-be-lazy (match tag ((LStruct) True) (_ False))))
      (Parser
       pn
       (fn (input context)
         (match (pf input context)
           ((Ok (Tuple3 v c i))
            (let ((registry (.registry context))
                  (lookup-id (make-lookup-id v tag)))
              (match (%hm:lookup registry lookup-id)
                ((Some _f)
                 (Ok (Tuple3 (Form FNNone (KLookup lookup-id)) c i)))
                ((None)
                 (if should-be-lazy
                     (progn
                       (traceobject ">>> WARNING: lookup failed for: " lookup-id)
                       (Ok (Tuple3 (Form FNNone (KAtom CTVoid)) c i)))
                     (Err (push-error
                           input
                           pn
                           "lookup failed: target form not found"
                           (make-error-stack))))
                 ))))
           ((Err e)
            (Err (push-trace pn "lookup failed in reader" e))))))))

  (declare must (Parser :a -> Parser :a))
  (define (must p)
    (let ((pf (get-parser p))
          (pn (mconcat (make-list "must[" (get-parser-name p) "]"))))
      (Parser
       pn
       (fn (input context)
         (match (pf input context)
           ((Ok r) (Ok r))
           ((Err e) 
            (Err (push-fatal input pn "must failed on fatal error" e))))))))

  (declare lazy ((Unit -> Parser :a) -> Parser :a))
  (define (lazy f)
    (Parser
     "lazy"
     (fn (input context)
       (get-parser (f) input context))))

  (declare replace (:a -> Parser :a))
  (define (replace value)
    (map (fn (_) value) read-symbol))

  ;; -------------------- VALUE PARSERS --------------------

  (declare atom-value (Parser Form))
  (define atom-value (map (fn (value) (Form FNNone (KAtom value))) read-ctype))

  (declare scalar-value (Parser Form))
  (define scalar-value (alt atom-value (lookup LTypeDef)))

  (declare inlined-struct (Unit -> (Parser Form)))
  (define (inlined-struct)
    (map (fn (f)
           (match f
             ((Form name _kind)
              (Form name (KInlined f)))))
         (lazy struct)))

  ;; TODO: self-recursive structs can't be looked up, so it would be nice
  ;; to keep context and check if this is the case instead of blindly
  ;; assuming we are dealing with a pointer we can safely set to :void
  (declare struct-compound (Parser CompoundValue))
  (define struct-compound
    (let ((lookup-struct (liftA2 (fn (_ f) (CVTarget f))
                                 (keyword? (%sym:make-keyword "struct"))
                                 (lookup LStruct)))
          (lookup-union (liftA2 (fn (_ f) (CVTarget f))
                                (keyword? (%sym:make-keyword "union"))
                                (lookup LUnion)))
          (lookup-enum (liftA2 (fn (_ f) (CVTarget f))
                               (keyword? (%sym:make-keyword "enum"))
                               (lookup LEnum))))
      (alt lookup-struct (alt lookup-union lookup-enum))))

  (declare bare-compound (Unit -> (Parser CompoundValue)))
  (define (bare-compound)
    (alt (open-list (alt struct-compound
                         (map CVTarget (lazy inlined-struct))))
         (map CVTarget scalar-value)))

  (declare indexed-compound ((Parser CompoundValue) -> (Parser CompoundValue)))
  (define (indexed-compound p)
    (liftA3 (fn (_ target elem-count) (CVArray target elem-count))
            (keyword? (%sym:make-keyword "array"))
            (must p)
            read-integer))

  (declare pointed-compound ((Parser CompoundValue) -> (Parser CompoundValue)))
  (define (pointed-compound p)
    (liftA2 (fn (_ target) (CVPointer target))
            (keyword? (%sym:make-keyword "pointer"))
            (must p)))

  (declare compound-any (Unit -> (Parser CompoundValue)))
  (define (compound-any)
    (alt (open-list
          (alt (pointed-compound (lazy compound-any))
               (indexed-compound (lazy compound-any))))
         (bare-compound)))

  (declare compound-value (Unit -> (Parser Form)))
  (define (compound-value)
    (map (fn (compound) (Form FNNone (KCompound compound)))
         (compound-any)))

  (declare value-any (Unit -> Parser Form))
  (define (value-any) (alt scalar-value (compound-value)))

  ;; -------------------- FORM PARAMETERS --------------------
  
  (declare form-param (String -> (Integer -> FormParam) -> (Parser FormParam)))
  (define (form-param kw-str ctor)
    (liftA2 (fn (_ value) (ctor value))
            (keyword? (%sym:make-keyword kw-str))
            read-integer))

  (define form-param-size (alt (form-param "size" PSize)
                               (form-param "bit-size" PSize)))
  (define form-param-offset (form-param "bit-offset" POffset))
  (define form-param-alignment (form-param "bit-alignment" PAlignment))

  (declare form-param-any (Parser FormParam))
  (define form-param-any
    (alt form-param-alignment (alt form-param-size form-param-offset)))

  ;; -------------------- STRUCT/UNION --------------------

  (declare struct-field (Unit -> (Parser StructField)))
  (define (struct-field)
    (liftA3 (fn (name (Form _name kind) params) (Tuple (Form name kind) params))
            form-name-sym (value-any) (many (fn (_index) form-param-any))))

  (declare struct (Unit -> (Parser Form)))
  (define (struct)
    (liftA2 (fn (tag (Tuple3 name size fields))
              (let ((kind (match tag
                            ("struct" (KStruct (make-list size) fields))
                            ("union" (KUnion fields))
                            (_ (error "not on my watch")))))
                (Form name kind)))
            (alt (string-icase? "struct") (string-icase? "union"))
            (must
             (liftA3 (fn (name size fields) (Tuple3 name size fields))
                     (alt form-name-id form-name-sym) ;; order matters, sym will eat :id kw
                     form-param-size
                     (many (fn (_index) (open-list (struct-field))))))))

  ;; -------------------- ENUM --------------------

  (declare enum-field (Parser EnumField))
  (define enum-field
    (liftA2 Tuple form-name-sym read-integer))

  (declare union (Parser Form))
  (define union
    (liftA3 (fn (_ name fields) (Form name (KEnum fields)))
            (string-icase? "enum")
            (alt form-name-id form-name-sym)
            (must (many (fn (_index) (open-list enum-field))))))

  ;; -------------------- TYPEDEF --------------------

  (declare typedef (Parser Form))
  (define typedef
    (liftA2 (fn (_ f) f)
            (string-icase? "typedef")
            (must
             (liftA2 (fn (name value) (Form name (KTypeDef value)))
                     form-name-sym
                     (alt (open-list (lazy inlined-struct)) (value-any))))))

  ;; -------------------- FUNCTION --------------------

  (declare function-arg (Integer -> Parser FunctionArg))
  (define (function-arg index)
    (let ((named-arg
            (liftA2 (fn (name (Form _name def)) (Form name def))
                    form-name-sym (value-any)))
          (anon-arg
            (map (fn ((Form _name def))
                   (let ((arg-name
                           (FNSymbol (%sym:make-symbol (<> "arg-" (into index))))))
                     (Form arg-name def)))
                 (value-any))))
      (alt named-arg anon-arg)))

  (declare function (Parser Form))
  (define function
    (liftA2 (fn (_ form) form)
          (string-icase? "function")
          (must
           (liftA3 (fn (name args (Form _name ret-val)) (Form name (KFunction args ret-val)))
                   form-name-string
                   (alt (empty-list (function-arg 0))
                        (open-list (many (fn (index) (open-list (function-arg index))))))
                   (value-any)))))

  ;; -------------------- CONST --------------------

  (declare const-value (Parser Form))
  (define const-value
    (liftA2 (fn (_ form) form)
            (string-icase? "const")
            (must
             (liftA3 (fn (name type value) (Form name (KConst (Tuple type value))))
                     form-name-sym
                     (lookup LTypeDef)
                     read-string))))

  ;; -------------------- TOP LEVEL --------------------

  (declare form-parser (Parser Form))
  (define form-parser (alt typedef (alt (struct) (alt function (alt union const-value)))))

)
