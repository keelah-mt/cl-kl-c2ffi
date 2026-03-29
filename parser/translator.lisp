(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native cl:t)
  (define-type CffiView)

  (define-instance (Semigroup CffiView)
    (define (<> a1 a2)
      (lisp CffiView (a1 a2)
        (cl:if (cl:null a1)
               (cl:list a2)
               (cl:if (cl:null a2)
                      (cl:list a1)
                      (cl:append (cl:list a1) (cl:list a2)))))))

  (define-instance (Monoid CffiView)
    (define mempty
      (lisp CffiView () (cl:list))))

  (declare << (CffiView -> CffiView -> CffiView))
  (define (<< a1 a2)
    (lisp CffiView (a1 a2)
      (cl:let ((l1 (cl:if (cl:listp a1) a1 (cl:list a1)))
               (l2 (cl:if (cl:listp a2) a2 (cl:list a2))))
        (cl:append l1 l2))))

  (repr :native cl:package)
  (define-type Package)

  (define-type TranslatorError (TranslatorError String))
  (define-type-alias TranslatorResult (Result TranslatorError (List CffiView)))

  (declare to-readtable-case (String -> String))
  (define (to-readtable-case str)
    (lisp String (str)
      (cl:ecase (cl:readtable-case cl:*readtable*)
        (:upcase (cl:string-upcase str))
        (:downcase (cl:string-downcase str))
        (:preserve str)
        (:invert str))))


  (define-type-alias NameTranslator (String -> String))

  (declare to-param-case NameTranslator)
  (define (to-param-case str)
    (lisp String (str)
      (cl:let ((pos (cl:or (cl:position #\_ str :test-not #'cl:char=) (cl:length str))))
        (cl:concatenate 'cl:string
                        (cl:make-string pos :initial-element #\%)
                        (param-case (cl:subseq str pos))))))

  (declare intern-symbol (Symbol -> Package -> NameTranslator -> CffiView))
  (define (intern-symbol sym package translator)
    (let ((translated (to-readtable-case (translator (%sym:symbol-name sym)))))
      (lisp CffiView (translated package)
        (cl:intern translated package))))

  (declare intern-keyword (Symbol -> NameTranslator -> CffiView))
  (define (intern-keyword kw translator)
    (let ((translated (to-readtable-case (translator (%sym:symbol-name kw)))))
      (lisp CffiView (translated)
        (cl:intern translated :keyword))))

  (declare string->view (String -> NameTranslator -> CffiView))
  (define (string->view str translator)
    (let ((translated
            (translator (lisp String (str)
                          (cl:concatenate 'cl:string (cl:string-trim "\"" str) "")))))
      (lisp CffiView (translated) translated)))

  (declare integer->view (Integer -> CffiView))
  (define (integer->view value)
    (lisp CffiView (value) value))

  (declare list->view ((List CffiView) -> CffiView))
  (define (list->view l)
    (lisp CffiView (l) l))

  (define-type (Scoped :a) (Scoped :a FormRank Package FormRegistry NameTranslator))

  (declare make-scoped (Package -> FormRegistry -> NameTranslator -> FormRank -> :a -> (Scoped :a)))
  (define (make-scoped package registry translator rank value)
    (Scoped value rank package registry translator))

  (declare into-scoped ((Into (Scoped :a) CffiView) => (Package -> FormRegistry -> NameTranslator -> FormRank -> :a -> CffiView)))
  (define (into-scoped package registry name-translator rank value)
    (into (make-scoped package registry name-translator rank value)))

  ;; TODO: in theory could use fraction and check division...
  (declare bits->bytes (Integer -> Integer))
  (define (bits->bytes bits)
    (%mreal:round/ bits 8))

  (declare constant-type? (CType -> Boolean))
  (define (constant-type? ct)
    (match ct
      ((CTVoid) False)
      ((CTFunctionPointer) False)
      (_ True)))

  (declare numeric-type? (CType -> Boolean))
  (define (numeric-type? ct)
    (match ct
      ((CTNative _ _) True)
      ((CTSystem _) True)
      ((CTFloat _) True)
      (_ False)))

  (declare integer-type? (CType -> Boolean))
  (define (integer-type? ct)
    (match ct
      ((CTNative _ _) True)
      ((CTSystem _) True)
      (_ False)))

  (declare string-type? (CType -> Boolean))
  (define (string-type? ct)
    (match ct
      ((CTNative (CNChar) _) True)
      (_ False)))

  (define-instance (Into (Scoped FormParam) CffiView)
  (define (into (Scoped param _rank package registry translator))
    (match param
      ((PSize value)
       (<> (intern-keyword (%sym:make-symbol "size") id)
           (integer->view (bits->bytes value))))
      ((POffset value)
       (<> (intern-keyword (%sym:make-symbol "offset") id)
           (integer->view (bits->bytes value))))
      ((PAlignment _value) mempty))))

  (declare get-offset-param ((List FormParam) -> FormParam))
  (define (get-offset-param params)
    (match (find (fn (p) (match p ((POffset _) True) (_ False))) params)
      ((Some p) p)
      ((None) (POffset 0))))

  (declare get-size-param ((List FormParam) -> FormParam))
  (define (get-size-param params)
    (match (find (fn (p) (match p ((PSize _) True) (_ False))) params)
      ((Some p) p)
      ((None) (PSize 0))))

  ;; TODO: and this should use Result too as FNNone is not a valid name to output 
  (define-instance (Into (Scoped FormName) CffiView)
    (define (into (Scoped name _rank package registry translator))
      (match name
        ((FNSymbol value)
         (intern-symbol value package translator))
        ((FNKeyword value)
         (intern-keyword value translator))
        ((FNId value)
         (intern-symbol (%sym:make-symbol (<> "id-" (into value))) package translator))
        ((FNString value)
         (string->view value translator))
        ((FNNone)
         (string->view "%%NONE" id)))))

  (define-instance (Into (Scoped CompoundValue) CffiView)
    (define (into (Scoped value rank package registry translator))
      (let ((in-scope (fn (x) (into-scoped package registry translator rank x))))
        (match value
          ((CVTarget (Form _ kind))
           (in-scope kind))
          ((CVPointer c)
           (<> (intern-keyword (%sym:make-symbol "pointer") id)
               (in-scope c)))
          ((CVArray c s)
           (<< (in-scope c)
               (<> (intern-keyword (%sym:make-symbol "count") id)
                   (integer->view s))))))))

  (declare array-view? (CffiView -> Boolean))
  (define (array-view? view)
    ;; hack #1 to compose views properly as arrays (at least in structs)
    ;; are not represented by compound values like absolutely anything else
    ;; and so :count param should be pasted right into a field def without enclosing ()
    (lisp Boolean (view)
      (cl:if (cl:listp view)
             (cl:if (cl:find :count view) True False)
             False)))

  (declare fix-array-fn-param (CffiView -> CffiView))
  (define (fix-array-fn-param view)
    ;; hack #2 function args don't take :count so the best thing is probably
    ;; to convert it to a pointer of the specified type
    (lisp CffiView (view)
      (cl:labels ((convert (view)
                    (cl:if (cl:listp view)
                           (cl:if (array-view? view)
                               `(:pointer ,(cl:first view))
                               (cl:map 'cl:list #'convert view))
                           view)))
        (convert view))))

  (define-instance (Into CType CffiView)
    (define (into ct)
      (match ct
        ((CTNative kind sign)
         (let ((prefix
                 (match sign
                   ((Unsigned) "unsigned-")
                   ((Signed) "")))
               (name
                 (match kind
                   ((CNChar) "char")
                   ((CNShort) "short")
                   ((CNInt) "int")
                   ((CNLong) "long")
                   ((CNLongLong) "long-long"))))
           (intern-keyword (%sym:make-symbol (<> prefix name)) id)))

        ((CTSystem kind)
         (let ((name
                 (match kind
                   ((CSI8) "int8")
                   ((CSU8) "uint8")
                   ((CSI16) "int16")
                   ((CSU16) "uint16")
                   ((CSI32) "int32")
                   ((CSU32) "uint32")
                   ((CSI64) "int64")
                   ((CSU64) "uint64")
                   ((CSSize) "size")
                   ((CSSSize) "ssize")
                   ((CSIntPtr) "intptr")
                   ((CSUIntPtr) "uintptr")
                   ((CSPtrDiff) "ptrdiff")
                   ((CSOffset) "offset"))))
           (intern-keyword (%sym:make-symbol name) id)))

        ((CTFloat prec)
         (let ((name
                 (match prec
                   ((FPSingle) "float")
                   ((FPDouble) "double")
                   ;; TODO: fail instead
                   ((FPLongDouble) "double"))))
           (intern-keyword (%sym:make-symbol name) id)))

        ((CTVoid)
         (intern-keyword (%sym:make-symbol "void") id))
        ((CTFunctionPointer)
         (intern-keyword (%sym:make-symbol "pointer") id)))))

  (declare resolve-form (FormRegistry -> RankedForm -> (Optional RankedForm)))
  (define (resolve-form registry (RankedForm rank (= f (Form _name kind))))
    (match kind
      ((KAtom _value)
       (Some (RankedForm rank f)))
      ((KCompound compound)
       (match compound
         ((CVTarget t)
          (resolve-form registry (RankedForm rank t)))
         ((CVPointer c)
          (resolve-form registry (RankedForm rank (Form (FNId 0) (KCompound c)))))
         ((CVArray _c _num)
          ;; TODO: not sure if arrays atm appear anywhere but struct fields
          ;; maybe consts? for now I am not going to bother...
          None)))
      ((KInlined i)
       ;; TODO: not sure, this form will get pulled out as a dep, so it can/should be resolved?
       (resolve-form registry (RankedForm rank i)))
      ((KLookup id)
       (match (lookup-form registry id (Some rank))
         ((Some l)
          (resolve-form registry (RankedForm rank l)))
         ((None)
          None)))
      ((KConst _value)
       None)
      ((KStruct params _fields)
       (let ((size
               (match (get-size-param params)
                 ((PSize value) value)
                 (_ 0))))
         (if (== 0 size)
             ;; don't resolve empty structs it just adds noise and obscures source types
             None
             (Some (RankedForm rank f)))))
      ((KUnion _fields)
       (Some (RankedForm rank f)))
      ((KEnum _fields)
       (Some (RankedForm rank f)))
      ((KFunction _args _retval)
       None)
      ((KTypeDef t)
       (resolve-form registry (RankedForm rank t)))))

  (declare kind->compound-tag (FormKind -> (Optional CffiView)))
  (define (kind->compound-tag kind)
    (match kind
      ((KStruct _ _)
       (Some (intern-keyword (%sym:make-symbol "struct") id)))
      ((KUnion _)
       (Some (intern-keyword (%sym:make-symbol "union") id)))
      (_ None)))

  ;; TODO: all this conversions should be converted to Result
  (define-instance (Into (Scoped FormKind) CffiView)
    (define (into (Scoped kind rank package registry translator))
      (let ((in-scope (fn (x) (into-scoped package registry translator rank x))))
        (match kind
          ((KAtom ct)
           (into ct))
          ((KTypeDef (Form _ kind))
           (in-scope kind))
          ((KCompound value)
           (in-scope value))
          ((KLookup f-id)
           (match (lookup-form registry f-id None)
             ((Some (= f (Form orig-name _)))
              (let ((resolved (resolve-form registry (RankedForm rank f)))
                    (final (match resolved
                             ((Some (RankedForm _rank rf)) rf)
                             ((None) f)))
                    (final-name (match resolved
                                  ((Some (RankedForm _rank (Form name _kind)))
                                   (match name
                                     ((FNNone) orig-name)
                                     (_ name)))
                                  ((None) orig-name))))
                (match final
                  ((Form name kind)
                   (match (kind->compound-tag kind)
                     ((Some tag)
                      (<> tag (in-scope final-name)))
                     ((None) (in-scope final-name)))))))
             ((None)
              (string->view (<> "NOT-FOUND:" f-id) id))))
          ((KInlined (Form name kind))
           (match (kind->compound-tag kind)
             ((Some tag)
              (<> tag (in-scope name)))
             ((None) (in-scope name))))
          (_ (string->view "KindlyLater!" id))))))

  (declare kind->cffi-tag (FormKind -> Package -> (Result TranslatorError CffiView)))
  (define (kind->cffi-tag kind package)
    (let ((to-tag
            (fn (value)
              (lisp CffiView (value)
                (cl:intern (to-readtable-case value) (cl:find-package :cffi)))))
          (to-const
            (lisp CffiView ()
              (cl:intern (to-readtable-case "define-constant") (cl:find-package :alexandria))))
          (not-top-level
            (fn (name)
              (Err (TranslatorError
                    (mconcat
                     (make-list
                      "Trying to convert " name " that is not a top-level form")))))))
      (match kind
        ((KAtom _)
         (not-top-level "KAtom"))
        ((KCompound _)
         (not-top-level "KCompound"))
        ((KLookup _)
         (not-top-level "KLookup"))
        ((KInlined (Form _ kind))
         (kind->cffi-tag kind package))
        ((KConst _)
         (Ok to-const))
        ((KStruct _ _)
         (Ok (to-tag "defcstruct")))
        ((KUnion _)
         (Ok (to-tag "defcunion")))
        ((KEnum _)
         (Ok (to-tag "defcenum")))
        ((KFunction _ _)
         (Ok (to-tag "defcfun")))
        ((KTypeDef _)
         (Ok (to-tag "defctype"))))))

  ;; TODO: not complete, needs testing
  (declare extract-deps (Form -> (List Form)))
  (define (extract-deps (Form _name kind))
    (let ((extract-fields (fn (fields)
                            (concatmap extract-deps
                                       (map (fn ((Tuple f _params)) f) fields)))))
      (match kind
        ((KAtom _)
         (make-list))
        ((KCompound value)
         (match value
           ((CVTarget f)
            (extract-deps f))
           ((CVPointer value)
            (extract-deps (Form (FNId 0) (KCompound value))))
           ((CVArray value _size)
            (extract-deps (Form (FNId 0) (KCompound value))))))
        ((KLookup _)
         ;; TODO:
         (make-list))
        ((KConst _)
         ;; NOTE: this probably doesn't make sense as const values must map to a bare CL type
         ;; that is encoded by KAtom, anything else is currently invalid
         (make-list))
        ((KInlined f)
         (append (extract-deps f) (make-list f)))
        ((KStruct _ fields)
         (extract-fields fields))
        ((KUnion fields)
         (extract-fields fields))
        ((KEnum _)
         (make-list))
        ((KFunction _ _)
         ;; TODO: deps in args/return values? check some creative wayland stuff
         (make-list))
        ((KTypeDef f)
         (extract-deps f)))))

  (declare form->view (RankedForm -> Package -> FormRegistry -> NameTranslator -> (Result TranslatorError CffiView)))
  (define (form->view (RankedForm rank (Form name kind)) package registry translator)
    (let ((in-scope
            (fn (x) (into-scoped package registry translator rank x)))
          (name-view
            (in-scope name))
          (fields-view
            (fn (fields w-offset)
              (list->view (map (fn ((Tuple (Form f-name f-kind) f-params))
                                 (let ((kind-view (in-scope f-kind)))
                                   (<< ((if (array-view? kind-view) << <>)
                                        (in-scope f-name) kind-view)
                                       (if w-offset
                                           (in-scope (get-offset-param f-params))
                                           mempty))))
                               fields)))))
      (do
       (tag <- (kind->cffi-tag kind package))
       (def <- (match kind
                 ((KAtom _) (Ok mempty))
                 ((KCompound _) (Ok mempty))
                 ((KInlined _) (Ok mempty))
                 ((KLookup _) (Ok  mempty))
                 ((KConst (Tuple f value))
                  (let ((resolved (resolve-form registry (RankedForm rank f))))
                    (match resolved
                      ((Some (RankedForm _r-rank (Form _r-name r-kind)))
                       (match r-kind
                         ((KAtom ctype)
                          (cond
                            ((numeric-type? ctype)
                             (bimap (fn (_err)
                                      (TranslatorError
                                       (mconcat (make-list "cannot resolve const:"
                                                           (into name)
                                                           ", cannot convert value to int: "
                                                           value))))
                              (fn (val)
                                (<> name-view (integer->view val)))
                                  (try-as Integer value)))
                            ((string-type? ctype)
                             (Ok (<> name-view (string->view value id))))
                            (True
                             (Err
                              (TranslatorError
                               (mconcat (make-list "cannot resolve const:"
                                                   (into name)
                                                   ", type cannot be used as a const type: "
                                                   (into r-kind))))))))
                         (_
                          (Err
                           (TranslatorError
                            (mconcat (make-list "cannot resolve const: "
                                                (into name)
                                                ", kind is not an atom type: "
                                                (into r-kind))))))))
                      ((None)
                       (Err
                        (TranslatorError (<> "cannot resolve const:" (into name))))))))
                 ((KStruct params fields)
                  (let ((header (<> mempty (<< name-view (in-scope (get-size-param params))))))
                    (Ok (<< header (fields-view fields True)))))
                 ((KUnion fields)
                  (Ok (<< name-view (fields-view fields False))))
                 ((KTypeDef f)
                  (let ((resolved (resolve-form registry (RankedForm rank f))))
                    (match resolved
                      ((Some (RankedForm _r-rank (Form _r-name r-kind)))
                       (match r-kind
                         ((KEnum _)
                          ;; don't emit enum typedefs useless, if not harmful
                          (Err
                           (TranslatorError (<> "not emitting typedef for enum:" (into name)))))
                         ;; TODO: what else
                         (_ (Ok (<> name-view (in-scope kind))))))
                      ((None)
                       (Ok (<> name-view (in-scope kind)))))))
                 ((KFunction args ret-val)
                  (let ((fn-name
                          (match name
                            ((FNString str)
                             (<> (string->view str id)
                                 (in-scope (FNSymbol (%sym:make-symbol str)))))
                            (_ name-view)))
                        (arg-list
                          (list->view (map (fn ((Form arg-name arg-kind))
                                             (<> (in-scope arg-name)
                                                 (fix-array-fn-param (in-scope arg-kind))))
                                           args))))
                    (Ok (<< (<> fn-name (in-scope ret-val)) arg-list))))
                 ((KEnum fields)
                  (Ok (<< name-view
                          (list->view (map (fn ((Tuple f-name f-value))
                                             (let ((kw-name
                                                     (in-scope
                                                      (match f-name
                                                        ((FNSymbol sym) (FNKeyword sym))
                                                        (_ f-name)))))
                                               (<> kw-name (integer->view f-value))))
                                           fields)))))))
        (pure (<< tag def)))))

  (declare form->definition (RankedForm -> Package -> FormRegistry -> NameTranslator -> (Result TranslatorError (List CffiView))))
  (define (form->definition (RankedForm rank f) package registry translator)
    (do
     ;; TODO: this extracts nested deps multiple times, perhaps extract-deps should stop
     ;; after depth 1 but this might not extract deps from compound values (can there be deps?)
     ;; or dupes needs to be tracked in some set accumulator
     (deps <- (mconcatmap (fn (dep)
                            (form->definition (RankedForm rank dep) package registry translator))
                          (extract-deps f)))
     (form-view <- (form->view (RankedForm rank f) package registry translator))
      (pure (append deps (make-list form-view)))))

  ;; TODO: return list of errors/warning instead of spamming here
  (declare convert-form (RankedForm -> Package -> FormRegistry -> NameTranslator -> (Result TranslatorError (List CffiView))))
  (define (convert-form f package registry translator)
    (form->definition f package registry translator))

  (declare map-with-index (((Tuple :a UFix) -> :b) -> (Vector :a) -> (List :b)))
  (define (map-with-index fa v)
    (let ((map-ranked
            (fn (lst rank)
              (match lst
                ((Cons a (= as _))
                 (cons (fa (Tuple a rank)) (map-ranked as (1+ rank))))
                (_
                 (make-list))))))
      (map-ranked (into v) 0)))

  ;; (declare translate-1 ((Vector String) -> (List (Tuple String String))))
  ;; (define (translate-1 v)
  ;;   (map-with-rank (fn ((Tuple s rank)) (Tuple s (into rank))) v))

  (declare translate (ParserContext -> Package -> NameTranslator -> TranslatorResult))
  (define (translate context package translator)
    (let ((registry (.registry context))
          (results
            (%lst:partition
             (fn (r) (match r ((Ok _) True) ((Err _) False)))
             (map-with-index
              (fn (l)
                ;; TODO: matching "l" to Tuple results in "deleting unreachable code"
                (let ((lookup-name (fst l))
                      (rank (snd l)))
                  (match (lookup-form registry lookup-name (Some rank))
                    ((Some f)
                     (convert-form (RankedForm rank f) package registry translator))
                    ((None)
                     (Err (TranslatorError
                           (mconcat (make-list "form: " lookup-name " not found"))))))))
              (.order context)
              ;; (%vec:singleton (%vec:head-unsafe (.order context)))
              ))))
      (coalton/experimental/loops:dolist
          (err (map (fn (err)
                      (match err
                        ((Err (TranslatorError e)) e)
                        (_ "")))
                    (snd results)))
        (traceobject ">>> TRANSLATION WARNING:" err))
      ;; TODO: why reverse???
      (mconcat (%lst:reverse (fst results)))))
)
