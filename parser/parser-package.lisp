(defpackage :cl-kl-c2ffi/parser
  (:nicknames :c2ffi/parser)
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:%str #:coalton/string)
                    (#:%lst #:coalton/list)
                    (#:%vec #:coalton/vector)
                    (#:%hm #:coalton/hashmap)
                    (#:%sym #:coalton/symbol)
                    (#:%mreal #:coalton/math/real))
  (:import-from #:coalton/symbol
                #:Symbol)
  (:import-from #:cl-change-case
                #:param-case)
  (:export
   :Form
   :RankedForm
   :Package
   :CffiView
   :InputView
   :NameTranslator
   :to-param-case
   :TranslatorResult
   :ParserContext
   :LookupTag
   :make-lookup-id
   :get-lookup-id
   :form-parser
   :make-empty-context
   :find-form
   :get-form-string
   :get-resolve-form-string
   :get-convert-form-string
   :feed-input
   :translate
   :find-form
   :get-find-form
   :resolve-kind
   :convert-form
   :get-convert-form
   :get-translation))
