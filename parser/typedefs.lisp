(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type ParserError
    (ParserTrace String)
    (ParserError String)
    (FatalError String))

  (define-type-alias ErrorStack (List ParserError))

  (declare make-error-stack (Unit -> (List ParserError)))
  (define (make-error-stack)
    (make-list))

  (declare push-trace (ParserName -> String -> ErrorStack -> ErrorStack))
  (define (push-trace name message stack)
    (cons (ParserTrace (mconcat (make-list name " trace: " message))) stack))

  (declare push-error (InputView -> ParserName -> String -> ErrorStack -> ErrorStack))
  (define (push-error input name message stack)
    (cons (ParserError
            (mconcat (make-list name " error at: " (into input) ", " message)))
          stack))

  (declare push-fatal (InputView -> ParserName -> String -> ErrorStack -> ErrorStack))
  (define (push-fatal input name message stack)
    (cons (FatalError
            (mconcat (make-list name " fatal error at: " (into input) ", " message)))
          stack))

  (declare any-fatal? (ErrorStack -> Boolean))
  (define (any-fatal? stack)
    (any (fn (e)
           (match e
             ((FatalError _) True)
             (_ False)))
         stack))
  
  (define-instance (Into ParserError String)
    (define (into e)
      (match e
        ((ParserTrace msg) (mconcat (make-list "  ->  " msg)))
        ((ParserError msg) (mconcat (make-list "  ->  " msg)))
        ((FatalError msg) (mconcat (make-list "  ->  " msg))))))

  ;; (define-instance (Into ErrorStack String)
  ;; (define (into s)
  ;;   (match (find (fn (e)
  ;;                  (match e
  ;;                    ((FatalError _msg) True)
  ;;                    (_ False)))
  ;;                s)
  ;;     ((Some fatal) (into fatal))
  ;;     ((None) (mconcatmap (fn (e) (mconcat (make-list "=> " (into e)))) (take 2 s))))))

  (define-instance (Into ErrorStack String)
  (define (into s)
    (<> "=> " (mconcatmap into s))))

  (define-type FormName
    (FNSymbol Symbol)
    (FNKeyword Symbol)
    (FNString String)
    (FNId Integer))

  (define-type Form (Form FormName FormKind))

  (define-type FormParam
    (PSize Integer)
    (POffset Integer)
    (PAlignment Integer))
  (define-type-alias FormParams (List FormParam))

  (define-type-alias StructField (Tuple Form FormParams))
  (define-type-alias EnumField (Tuple3 FormName Integer FormParams))
  (define-type-alias FunctionArg Form)
  (define-type-alias FunctionRetVal FormKind)

  (define-struct CompoundValue
    (target FormKind)
    (ptr-count Integer)
    (elem-count Integer))

  (define-type FormKind
    (KAtom Symbol)
    (KCompound CompoundValue)
    (KStruct FormParams (List StructField))
    (KUnion (List StructField))
    (KEnum (List EnumField))
    (KFunction (List FunctionArg) FunctionRetVal)
    (KTypeDef Form))

  (define-type-alias FormRegistry (%hm:hashmap String Form))

  (define-struct ParserContext
    (registry FormRegistry)
    (order (List FormName)))

  (define-type-alias (ParserState :a) (Result ErrorStack (Tuple3 :a ParserContext InputView)))
  (define-type-alias (ParserFn :a) (InputView -> ParserContext -> (ParserState :a)))
  (define-type-alias ParserName String)
  (define-type (Parser :a) (Parser ParserName (ParserFn :a)))

  (declare get-parser ((Parser :a) -> (ParserFn :a)))
  (define (get-parser (Parser _ f)) f)

  (declare get-parser-name ((Parser :a) -> ParserName))
  (define (get-parser-name (Parser name _)) name)

  (define-instance (Functor Parser)
    (define (map f p)
      (let ((pf (get-parser p))
            (pn (get-parser-name p)))
        (Parser
         pn
         (fn (input context)
           (match (pf input context)
             ((Ok (Tuple3 v c i)) (Ok (Tuple3 (f v) c i)))
             ((Err e) (Err e))))))))

  (define-instance (Applicative Parser)
    (define (pure a)
      (Parser
       "pure"
       (fn (input context)
         (Ok (Tuple3 a context input)))))

    (define (lifta2 f a b)
      (let ((pa (get-parser a))
            (na (get-parser-name a))
            (pb (get-parser b))
            (nb (get-parser-name b)))
        (Parser
         (mconcat (make-list na "+" nb))
         ;;"comb"
         (fn (input context)
           (match (pa input context)
             ((Ok (Tuple3 va ca ia))
              (match (pb ia ca)
                ((Ok (Tuple3 vb cb ib))
                 (Ok (Tuple3 (f va vb) cb ib)))
                ((Err e) (Err e))))
             ((Err e) (Err e))))))))

  (define-instance (Alternative Parser)
    (define (alt a b)
      (let ((pa (get-parser a))
            (na (get-parser-name a))
            (pb (get-parser b))
            (nb (get-parser-name b)))
        (Parser
         (mconcat (make-list "alt(" na "|" nb ")"))
         ;;"alt"
         (fn (input context)
           (match (pa input context)
             ((Ok result)
              (Ok result))
             ((Err e)
              (if (any-fatal? e)
                  (Err e)
                  (match (pb input context)
                    ((Ok result) (Ok result))
                    ((Err e) (Err e))))))))))

    (define empty
      (Parser
       "alt-empty"
       (fn (input _context)
         (Err (push-error input "alt-empty" "cannot match" (make-error-stack)))))))

  (declare lifta3 (Applicative :f => (:a -> :b -> :c -> :d) -> :f :a -> :f :b -> :f :c -> :f :d))
  (define (lifta3 f a b c)
    (lifta2 (fn (f-ab c-val) (f-ab c-val)) 
            (lifta2 f a b) 
            c))

  (declare liftA4 (Applicative :f => (:a -> :b -> :c -> :d -> :e)  -> :f :a -> :f :b -> :f :c -> :f :d -> :f :e))
  (define (liftA4 f a b c d)
    (liftA2 (fn (f-abc d-val) (f-abc d-val))
            (liftA3 f a b c)
            d))

)
