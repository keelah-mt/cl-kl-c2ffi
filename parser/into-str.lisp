(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare to-str (:a -> String))
  (define (to-str s)
    (lisp String (s) (cl:format nil "~S" s)))

  (declare intersperse-into (Into :a String => String -> (List :a) -> String))
  (define (intersperse-into s as)
    (mconcat (%lst:intersperse s (map into as))))

  (define-instance (Into FormParam String)
    (define (into fp)
      (match fp
        ((PSize value) (<> "SIZE:" (into value)))
        ((POffset value) (<> "OFFSET:" (into value)))
        ((PAlignment value) (<> "ALIGN:" (into value))))))

  (define-instance (Into CompoundValue String)
    (define (into cv)
      (match cv
        ((CVTarget f) (mconcat (make-list "@T:[" (into f) "]")))
        ((CVPointer value) (<> "@P:" (into value)))
        ((CVArray value size) (mconcat (make-list  "@A[" (into size) "]:" (into value)))))))

  (define-instance (Into Form String)
    (define (into (Form name kind))
      (mconcat (make-list (into name) "->" (into kind)))))

  (define-instance (Into RankedForm String)
    (define (into (RankedForm rank f))
      (mconcat (make-list (into rank) ":" (into f)))))

  (define-instance (Into FormKind String)
    (define (into k)
      (match k
        ((KAtom sym)
         (<> "#KAtom:" (to-str sym)))
        ((KCompound value)
         (<> "#KCompound:" (into value)))
        ((KLookup lookup-id)
         (<> "#KLookup:" lookup-id))
        ((KConst (Tuple type value))
         (mconcat (make-list "#KConst:[@TYPE<" (into type) ">" (into value) "]")))
        ((KStruct params fields)
         (mconcat
          (make-list
           "#KStruct:[" (intersperse-into ";" params)"]"
           "@F:["
           (intersperse-into
            "|"
            (map (fn ((Tuple f p))
                   (mconcat (make-list
                             (the String (into f))
                             "<" (intersperse-into ";" p) ">")))
                 fields))
           "]")))
        ((KUnion _fields) (<> "#KUnion:" "TODO"))
        ((KEnum fields)
         (mconcat
          (make-list
           "#KEnum:@F:["
           (intersperse-into
            "|"
            (map (fn ((Tuple f v)) (mconcat (make-list (into f) ":" (into v))))
                 fields))
           "]")))
        ((KFunction args ret)
         (mconcat
          (make-list
           "#KFunction:[@RET<" (into ret) ">"
           "@ARG<" (intersperse-into "|" args) ">"
           "]")))
        ((KInlined f) (<> "#KInlined:" (into f)))
        ((KTypeDef f) (<> "#KTypeDef:" (into f))))))
)
