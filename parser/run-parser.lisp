(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare find-form (String -> ParserContext -> (Optional RankedForm)))
  (define (find-form lookup-name context)
    (let ((forms (%hm:lookup (.registry context) lookup-name)))
      (match forms
        ((Some (Cons rf _)) (Some rf))
        (_ None))))

  (declare get-find-form (String -> ParserContext -> RankedForm))
  (define (get-find-form lookup-name context)
    (match (find-form lookup-name context)
      ((Some value) value)
      ((None) (error "not found"))))

  (declare get-form-string (String -> ParserContext -> String))
  (define (get-form-string name context)
    (into (get-find-form name context)))

  (declare feed-input (ParserContext -> InputView -> ParserContext))
  (define (feed-input context view)
    (let ((p (get-parser form-parser)))
      (match (p view context)
        ((Ok (Tuple3 f _ctx _remainder))
         (update-context context f))
        ((Err e)
         (error (the String (into e)))))))

)
