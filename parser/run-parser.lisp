(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define (make-form-registry) (the (%hm:HashMap String Form) %hm:empty))

  (define (make-empty-context)
    (ParserContext (make-form-registry) (make-list)))

  (declare update-context (ParserContext -> Form -> ParserContext))
  (define (update-context ctx f)
    (match f
      ((Form name _)
       (ParserContext (%hm:insert (.registry ctx) (into name) f)
                      (cons name (.order ctx))))))

  (declare feed-input (ParserContext -> InputView -> ParserContext))
  (define (feed-input context view)
    (let ((p (get-parser form-parser)))
      (match (p view context)
        ((Ok (Tuple3 f _ctx _remainder))
         (update-context context f))
        ((Err e)
         (error (the String (into e)))))))

  (declare translate (ParserContext -> String -> (List Unit)))
  (define (translate _context _pkg)
    (make-list))

)
