(in-package :c2ffi/parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare validate ((:a -> (Optional String)) -> Parser :a -> Parser :a))
  (define (validate f p)
    (let ((pf (get-parser p))
          (pn (mconcat (make-list "(validate:" (get-parser-name p) ")"))))
      (Parser
       pn
       (fn (input state)
         (match (pf input state)
           ((Ok (Tuple3 v c i))
            (match (f v)
              ((None)
               (Ok (Tuple3 v c i)))
              ((Some str)
               (Err (push-error input
                                pn
                                (mconcat (make-list "expected: " str))
                                (make-error-stack))))))
           ((Err e) (Err (push-trace pn "validate failed in reader" e))))))))

  (declare read-symbol (Parser Symbol))
  (define read-symbol
    (let ((pn "read-symbol"))
      (Parser
       pn
       (fn (input state)
         (bimap (fn (e) (push-error input pn e (make-error-stack)))
                (fn (result) (Tuple3 (fst result) state (snd result)))
                (%take-symbol input))))))

  (declare read-keyword (Parser Symbol))
  (define read-keyword
    (validate (fn (v)
                (if (%sym:keyword? v)
                    None
                    (Some "keyword")))
              (Parser "read-keyword" (get-parser read-symbol))))

  (declare read-string (Parser String))
  (define read-string
    (let ((pn "read-string"))
      (Parser
       pn
       (fn (input state)
         (bimap (fn (e) (push-error input pn e (make-error-stack)))
                (fn (result) (Tuple3 (fst result) state (snd result)))
                (%take-string input))))))

  (declare read-integer (Parser Integer))
  (define read-integer
    (let ((pn "read-integer"))
      (Parser
       pn
       (fn (input state)
         (bimap (fn (e) (push-error input pn e (make-error-stack)))
                (fn (result) (Tuple3 (fst result) state (snd result)))
                (%take-integer input))))))

  (define (string-icase? str)
    (let ((dcase (%str:downcase str)))
      (validate (fn (v) (if (== v dcase) None (Some dcase)))
                (map %str:downcase read-string))))

  (declare keyword? (Symbol -> (Parser Symbol)))
  (define (keyword? kw)
    (validate (fn (v)
                (let ((v-str (%str:downcase (%sym:symbol-name v)))
                      (kw-str (%str:downcase (%sym:symbol-name kw))))
                  (if (== v-str kw-str)
                      None
                      (Some (<> "keyword: :" (%sym:symbol-name kw))))))
              (Parser (<> "keyword?" (%sym:symbol-name kw)) (get-parser read-keyword))))

  (declare open-list ((Parser :a) -> (Parser :a)))
  (define (open-list p)
    (let ((pf (get-parser p))
          ;;(pn (mconcat (make-list "open-list(" (get-parser-name p) ")")))
          (pn "open-list")
          )
      (Parser
       pn
       (fn (input state)
         (match (%take-list input)
           ((Ok (Tuple list tail))
            (match (pf list state)
              ;; TODO: check list is consumed?
              ;; TODO: which context should be passed here?
              ((Ok (Tuple3 v c _list-eof))
               (Ok (Tuple3 v c tail)))
              ((Err e) (Err e))))
           ((Err e) (Err (push-error input pn e (make-error-stack)))))))))

  (declare empty-list ((Parser :a) -> (Parser (List :a))))
  (define (empty-list _p)
    (Parser
     "empty-list"
     (fn (input state)
       (match (%empty-list input)
         ((Ok (Tuple _list tail))
          (Ok (Tuple3 (make-list) state tail)))
         ((Err e)
          (Err (push-error input "empty-list" e (make-error-stack))))))))

  (declare many ((Parser :a) -> (Parser (List :a))))
  (define (many p)
    (let ((pf (get-parser p))
          (pn (get-parser-name p))
          (iter (fn (input context)
                  (match (pf input context)
                    ((Err _) (Tuple3 Nil context input))
                    ((Ok (Tuple3 va ca ia))
                     (match (iter ia ca)
                       ((Tuple3 vb cb ib) (Tuple3 (Cons va vb) cb ib))))))))
      (Parser
       (<> "many:" pn)
       (fn (input context)
         (Ok (iter input context))))))

)
