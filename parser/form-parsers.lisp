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

  (declare lookup-form (Parser FormKind))
  (define lookup-form
    (let ((pf (get-parser read-symbol))
          (pn "lookup-form"))
      (Parser
       pn
       (fn (input context)
         (match (pf input context)
           ((Ok (Tuple3 v c i))
            (let ((registry (.registry context)))
              (match (%hm:lookup registry (into (FNSymbol v)))
                ((Some (Form _name kind))
                 (Ok (Tuple3 kind c i)))
                ((None)
                 (Err (push-error
                       input
                       pn
                       "lookup failed: target form not found"
                       (make-error-stack)))))))
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

  ;; -------------------- VALUE PARSERS --------------------

  (declare atom-value (Parser FormKind))
  (define atom-value (map (fn (value) (KAtom value)) read-keyword))

  (declare scalar-value (Parser FormKind))
  (define scalar-value (alt atom-value lookup-form))

  ;; TODO: handle nesting in compound values

  (declare array-compound (Parser CompoundValue))
  (define array-compound
    (liftA3 (fn (_ target elem-count) (CompoundValue target 0 elem-count))
            (keyword? (%sym:make-keyword "array")) scalar-value read-integer))

  (declare pointer-compound (Parser CompoundValue))
  (define pointer-compound
    (liftA2 (fn (_ target) (CompoundValue target 1 0))
            (keyword? (%sym:make-keyword "pointer"))
            (must (alt scalar-value compound-value))))

  (declare struct-compound (Parser CompoundValue))
  (define struct-compound
    (liftA2 (fn (_ target) (CompoundValue target 0 0))
            (alt (keyword? (%sym:make-keyword "struct"))
                 (keyword? (%sym:make-keyword "union")))
            lookup-form))
  
  (declare compound-value (Parser FormKind))
  (define compound-value
    (map KCompound
         (open-list (alt array-compound (alt pointer-compound struct-compound)))))

  (declare value-any (Parser FormKind))
  (define value-any (alt scalar-value compound-value))

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

  (declare struct-field (Parser StructField))
  (define struct-field
    (liftA3 (fn (name kind params) (Tuple (Form name kind) params))
            form-name-sym value-any (many form-param-any)))

  (declare struct (Parser Form))
  (define struct
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
                     (many (open-list struct-field))))))

  ;; -------------------- TYPEDEF --------------------

  (declare typedef (Parser Form))
  (define typedef
    (liftA2 (fn (_ f) f)
            (string-icase? "typedef")
            (must
             (liftA2 (fn (name value) (Form name (KTypeDef value)))
                     form-name-sym
                     (alt (map (fn (value) (Form (FNId 0) value)) value-any) ;; TODO:
                          (open-list struct))))))
  ;; TODO: inline forms like struct above have to be marked specifically
  ;; as they have to be emitted first before e.g. typedef that inlines it

  ;; -------------------- FUNCTION --------------------

  (declare function-arg (Parser FunctionArg))
  (define function-arg
    (liftA2 Form form-name-sym value-any))

  (declare function (Parser Form))
  (define function
    (liftA2 (fn (_ form) form)
          (string-icase? "function")
          (must
           (liftA3 (fn (name args ret-val) (Form name (KFunction args ret-val)))
                   form-name-string
                   (alt (empty-list function-arg) (open-list (many (open-list function-arg))))
                   value-any))))

  ;; -------------------- TOP LEVEL --------------------

  (declare form-parser (Parser Form))
  (define form-parser (alt typedef (alt struct function)))

)
