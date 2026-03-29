(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare get-translation (TranslatorResult -> (List CffiView)))
  (define (get-translation tr)
    (match tr
      ((Ok value) value)
      ((Err (TranslatorError e)) (error e))))

  (declare get-resolve-form-string (RankedForm -> ParserContext -> String))
  (define (get-resolve-form-string (RankedForm rank f) context)
    (match (resolve-form (.registry context) (RankedForm rank f))
      ((Some (RankedForm r-rank r-f))
       (mconcat (make-list (into r-rank) ":" (into (get-lookup-id r-f)))))
      ((None)
       (mconcat (make-list (into rank) ":" (into (get-lookup-id f)))))))

  (declare get-convert-form (RankedForm -> Package -> NameTranslator -> ParserContext -> (List CffiView)))
  (define (get-convert-form f package translator context)
    (let ((registry (.registry context)))
      (match (convert-form f package registry translator)
        ((Ok value) value)
        ((Err (TranslatorError e)) (error e)))))

  (declare get-convert-form-string (RankedForm -> Package -> ParserContext -> String))
  (define (get-convert-form-string f package context)
    (let ((view (list->view (get-convert-form f package id context))))
      (lisp String (view)
        (cl:let ((cl:*print-pretty* cl:nil))
          (cl:format nil "~S" view)))))
)
