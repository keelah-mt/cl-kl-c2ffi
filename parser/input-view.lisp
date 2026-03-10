(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type-alias InputView (List Unit))

  (define-instance (Into InputView String)
    (define (into input)
      (lisp String (input)
        (cl:format cl:nil "~S" input))))

  (define-type-alias InputError String)

  (declare %take-symbol (InputView -> (Result InputError (Tuple Symbol InputView))))
  (define (%take-symbol input)
    (catch (Ok (lisp (Tuple Symbol InputView) (input)
                   (cl:let ((el (cl:car input)))
                     (cl:if (cl:symbolp el)
                            (Tuple el (cl:cdr input))
                            (cl:error "not a symbol")))))
      (_ (Err "not a symbol"))))

  (declare %take-string (InputView -> (Result InputError (Tuple String InputView))))
  (define (%take-string input)
    (catch (Ok (lisp (Tuple String InputView) (input)
                   (cl:let ((el (cl:car input)))
                     (cl:if (cl:null el)
                            (cl:error "eof")
                            (Tuple (cl:format cl:nil "~S" el) (cl:cdr input))))))
      (_ (Err "eof"))))

  (declare %take-integer (InputView -> (Result InputError (Tuple Integer InputView))))
  (define (%take-integer input)
    (catch (Ok (lisp (Tuple Integer InputView) (input)
                 (cl:let ((el (cl:car input)))
                   (cl:if (cl:integerp el)
                          (Tuple el (cl:cdr input))
                          (cl:error "eof")))))
      (_ (Err "cannot read integer"))))

  (declare %take-list (InputView -> (Result InputError (Tuple InputView InputView))))
  (define (%take-list input)
    (catch (Ok (lisp (Tuple InputView InputView) (input)
                 (cl:let ((el (cl:car input)))
                   (cl:if (cl:consp el)
                          (Tuple el (cl:cdr input))
                          (cl:error "not a list")))))
      (_ (Err "input is not a list"))))

  (declare %empty-list (InputView -> (Result InputError (Tuple InputView InputView))))
  (define (%empty-list input)
    (catch (Ok (lisp (Tuple InputView InputView) (input)
                 (cl:let ((el (cl:car input)))
                   (cl:if (cl:null el)
                          (Tuple '() (cl:cdr input))
                          (cl:error "not an empty list")))))
      (_ (Err "input is not an empty list"))))


)
