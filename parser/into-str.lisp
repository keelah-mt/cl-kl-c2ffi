(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-instance (Into FormName String)
    (define (into form-name)
      (let ((to-str (fn (s)
                      (lisp String (s) (cl:format nil "~S" s)))))
        (match form-name
          ((FNSymbol sym) (<> "[SYM]" (to-str sym)))
          ((FNKeyword kw) (<> "[KW]" (to-str kw)))
          ((FNString str) (<> "[STR]" str))
          ((FNId id) (<> "[ID]" (into id)))))))

  ;; TODO: it seems definitions can't be mutually recursive

  ;; (define-instance (Into CompoundValue String)
  ;;   (define (into c)
  ;;     (mconcat (make-list
  ;;               "("
  ;;               "P:" (into (.ptr-count c)) ","
  ;;               (into (.target c)) ","
  ;;               "C:"(into (.elem-count c))
  ;;               ")"))))

  ;; (define-instance (Into FormKind String)
  ;;   (define (into k)
  ;;     (match k
  ;;       ((KAtom) "#KAtom")
  ;;       ((KCompound value) (<> "#KCompound" (into value)))
  ;;       ((KStruct params fields) (mconcat (make-list "#KStruct:" (into params) (into fields))))
  ;;       ((KUnion _) "#KUnion:")
  ;;       ((KEnum _) "#KEnum:")
  ;;       ((KTypeDef f) (<> "#KTypeDef:" (into f))))))

  ;; (define-instance (Into Form String)
  ;;   (define (into f)
  ;;     (match f
  ;;       ((Form name kind)
  ;;        (mconcat (make-list (into name) "->" (into kind)))))))

  ;; (define-instance (Into FormParam String)
  ;;   (define (into p)
  ;;     (match p
  ;;       ((PSize value) (mconcat (make-list  "|SIZE:" (into value) "|")))
  ;;       ((POffset value) (mconcat (make-list "|OFFSET:" (into value) "|")))
  ;;       ((PAlignment value) (mconcat (make-list "|ALIGN:" (into value) "|"))))))

  ;; (define-instance (Into FormParams String)
  ;;   (define (into ps)
  ;;     (mconcatmap into ps)))

  ;; (define-instance (Into StructField String)
  ;;   (define (into (Tuple3 name form params))
  ;;     (mconcat (make-list "FIELD:" (into name) (into form) (into params)))))

  ;; (define-instance (Into StructFields String)
  ;;   (define (into fs)
  ;;     (mconcatmap into fs)))
)
