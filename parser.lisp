(defpackage :cl-kl-c2ffi/parser
  (:nicknames :c2ffi/parser)
  (:use #:cl)
  (:import-from :alexandria
                :eswitch)
  (:import-from :trivia
                :match
                :guard)
  (:import-from :serapeum/bundle
                :defconstructor
                :defunion
                :match-of)
  (:export :form-registry
           :make-form-registry
           :registry-add-form
           :registry-find-form
           :registry-lookup-form-id
           :registry-lookup-form
           :registry-ordered-forms
           :form-id
           :form-id-strict
           :form-name
           :fn-string
           :fn-keyword
           :fn-id
           :form-name->string
           :form-type
           :form-type-tag
           :get-ft-typename
           :atom-type-p
           :atom-type
           :ft-typedef-alias
           :ft-typedef-alias-target
           :ft-typedef-alias-ptr-counter
           :ft-struct
           :ft-enum
           :ft-const
           :ft-function
           :form-def
           :form-def-name
           :form-def-data
           :parse-form))

(in-package :c2ffi/parser)

;; -------------------- UTILS --------------------

(defmacro defunion-with-tags (name &rest variants)
  (let* ((variant-names (mapcar #'car variants))
         (tag-name (intern (format nil "~A-TAG" name) (symbol-package name))))
    `(progn
       (serapeum:defunion ,name ,@variants)
       (eval-when (:compile-toplevel :load-toplevel :execute)                             
         (deftype ,tag-name ()
           '(member ,@variant-names))))))

;; -------------------- FORM DEFINITION --------------------

(defun form-id-p (id)
  (or (integerp id) (null id)))

(defun form-id-strict-p (id)
  (integerp id))

(deftype form-id () '(satisfies form-id-p))
(deftype form-id-strict () '(satisfies form-id-strict-p))

(defunion form-name
  (fn-string (value string))
  (fn-keyword (value string))
  (fn-id (value integer)))

(defmethod print-object ((fn fn-string) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "~A[~A]" (class-name (class-of fn)) (fn-string-value fn))))

(defmethod print-object ((fn fn-keyword) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "~A[~A]" (class-name (class-of fn)) (fn-keyword-value fn))))

(defmethod print-object ((fn fn-id) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "~A[~D]" (class-name (class-of fn)) (fn-id-value fn))))

(defun form-name->string (fn)
  (match-of form-name fn
    ((fn-string value) value)
    ((fn-keyword value) (format nil "~S" value))
    ((fn-id value) (format nil "~D" value))))

(defun symbol->fn-keyword (sym)
  (fn-keyword (symbol-name sym)))

(defun symbol->form-name (sym)
  (fn-string (symbol-name sym)))

(defun get-form-name (form)
  "Get form name as a FN-STRING or FN-ID.

Most of the time the first symbol of a form is its name, in rare cases there might be
e.g. a nameless struct, it gets and :ID assigned, unfortunately the first form is not a
plist, so this requires manual handling."
  (if (keywordp (first form))
      (fn-id (getf form :id))
      (symbol->form-name (first form))))

;; NOTE: :pointer is omitted, it is handled by ft-alias
(defun atom-type-p (kw)
  (find kw '(:char :unsigned-char :short :unsigned-short :int :unsigned-int
             :long :unsigned-long :long-long :usigned-long-long :uchar :ushort
             :uint :ulong :llong :ullong :int8 :uint8 :int16 :uint16 :int32
             :uint32 :int64 :uint64 :size :ssize :intptr :uintptr :ptrdiff
             :offset :float :double :long-double :void)))

(deftype atom-type () '(satisfies atom-type-p))

(defunion-with-tags form-type
  (ft-typedef-alias
   (ptr-counter integer)
   (target (or form-id-strict atom-type))
   ;; length for static arrays
   (elem-count integer))
  (ft-struct
   (size number)
   ;; (<form-def>, (:offset, :size, :alignment in bytes))
   (fields list))
  (ft-enum
   ;; (:name <int>)
   (members list))
  (ft-const
   (type (or form-id-strict atom-type))
   (value t))
  (ft-function
   (ret-type form-def)
   ;; (<form-def>)
   (args list)))

(defun get-ft-typename (ft)
  (class-name (class-of ft)))

(defmethod print-object ((self ft-typedef-alias) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "~A[~D,~A]"
              (get-ft-typename self)
              (ft-typedef-alias-ptr-counter self)
              (ft-typedef-alias-target self))))

(defconstructor form-def
  (id form-id-strict)
  (name form-name)
  (data form-type))

(defmethod print-object ((fd form-def) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "[~D]:~A -> ~A" (form-def-id fd)  (form-def-name fd) (form-def-data fd))))

(defmethod fd-lookup-name ((self form-def))
  (format nil "~A:~A" (get-ft-typename (form-def-data self)) (form-def-name self)))

(defun make-fd-lookup-name (ftype fname)
  (declare (type form-type-tag ftype)
           (type form-name fname))
  (format nil "~A:~A" (symbol-name ftype) fname))

;; -------------------- FORM REGISTRY --------------

;; TODO: unfortunately a proper solution turned out to be elusive and so the registration
;; of multiple versions of the same type (e.g. a struct is first seen as a forward decl
;; and then as a full decl) is an afterthought and is handled by messing with form IDs
;; the result is - it breaks pure interface and is quite fragile overall. Something to redesign
;; once I am sufficiently recharged...

(defclass form-registry ()
  ((id-hash :initform (make-hash-table) :reader id-hash :type hash-table)
   (form-lookup :initform (make-hash-table :test 'equal) :reader form-lookup :type hash-table)
   (inserted-forms :initform (list) :reader registry-insterted-forms :type list)
   (last-id :initform 0 :reader last-id :type integer)))

(defun make-form-registry ()
  (make-instance 'form-registry))

;; TODO: needs testing, especially lookups with rank

(defmethod registry-add-form ((registry form-registry) form-name form-type)
  (with-slots (id-hash form-lookup inserted-forms last-id) registry
    (let* ((form-id (incf last-id))
           (form (form-def form-id form-name form-type)))
      (setf (gethash form-id id-hash) form)
      (push form-id inserted-forms)
      (push form-id (gethash (fd-lookup-name form) form-lookup))
      form)))

(defmethod registry-replace-form ((registry form-registry) replace-id data)
  (declare (type form-id-strict replace-id)
           (type form-type data))
  (with-slots (id-hash) registry
    (let* ((form (gethash replace-id id-hash))
           (new-form (form-def replace-id (form-def-name form) data)))
      (assert (eq (get-ft-typename (form-def-data form)) (get-ft-typename data)) (data)
              "Cannot replace form data with different type, expected ~S, got ~S"
              (get-ft-typename (form-def-data form))
              (get-ft-typename data))
      (setf (gethash replace-id id-hash) new-form))))

(defmethod registry-find-form ((registry form-registry) fid)
  (declare (type form-id-strict fid))
  (with-slots (id-hash) registry
    (gethash fid id-hash)))

(defmethod registry-lookup-form-id ((registry form-registry) ftype fname &key below-rank)
  (with-slots (form-lookup) registry
    (let ((lookup-list (gethash (make-fd-lookup-name ftype fname) form-lookup)))
      (if below-rank
          (find-if (lambda (v) (< v below-rank)) lookup-list)
          (first lookup-list)))))

(defmethod registry-lookup-form ((registry form-registry) ftype fname)
  (let ((form-id (registry-lookup-form-id registry ftype fname)))
    (when form-id
      (with-slots (id-hash) registry
        (values (gethash form-id id-hash) form-id)))))

(defmethod registry-ordered-forms ((registry form-registry))
  (reverse (slot-value registry 'inserted-forms)))

;; -------------------- PARSING --------------------

(defun parse-typedef (registry form &key register)
  (labels ((lookup-type (ftype sym)
             (registry-lookup-form-id registry ftype (symbol->form-name sym)))
           (match-typedef (typedef ptr-counter)
             (match typedef
               ;; cffi doesn't have signed-chat so will fake it
               (:signed-char
                (ft-typedef-alias ptr-counter :char 1))

               ;; SCL is the only lisp with support for this
               ;; TODO: so ignoring this for, very low on my priority list
               (:long-double
                (ft-typedef-alias 0 :double 1))

               ;; TODO: what to do with this?
               (:function-pointer
                (ft-typedef-alias 1 :void 1))

               ;; a simple atom e.g. :char
               ((and (type atom-type) a)
                (ft-typedef-alias ptr-counter a 1))

               ;; a simple alias e.g. aliased_type
               ;; can be for atom or for another alias (aliasN -> alias1 -> atom)
               ((and (type symbol) s)
                (ft-typedef-alias ptr-counter (lookup-type 'ft-typedef-alias s) 1))

               ;; explicit (:struct NAME) defs come from C's typedef struct...
               ;; or when C typdef was't defined, so struct gets typed explicitly
               ((list :struct (and (type symbol) s))
                (ft-typedef-alias ptr-counter (lookup-type 'ft-struct s) 1))

               ;; a enum alias
               ((list :enum (and (type symbol) s))
                (ft-typedef-alias ptr-counter (lookup-type 'ft-enum s) 1))

               ;; a pointed type:
               ;; - (:pointer :char),
               ;; - (:pointer alias),
               ;; - (:pointer (:struct some_struct))
               ;; or even (:pointer (:pointer (... something)))

               ((list :pointer (and (type symbol) s))
                (ft-typedef-alias
                 (1+ ptr-counter)
                 (if (keywordp s)
                     (progn
                       (unless (atom-type-p s)
                         (error "Pointer to a non-atom type ~S in form ~S" s typedef))
                       s)
                     (lookup-type 'ft-typedef-alias s))
                 1))

               ((list :pointer (list :struct (and (type symbol) s)))
                (ft-typedef-alias (1+ ptr-counter) (lookup-type 'ft-struct s) 1))

               ((list* :pointer tail) (match-typedef (car tail) (1+ ptr-counter)))

               ;; and weird types follow here:
               ;; 1. typedefs to a forward/undefined struct, usually just an opaque pointer,
               ;;    but can be a struct inlined in the typedef, these have :ID <NUMBER> as a name
               ((list* (and (type symbol)
                            (satisfies (lambda (x) (string-equal (symbol-name x) "struct"))))
                       struct-form)
                ;; TODO: very unfortunate but this design makes it hard to keep things pure.
                ;; since we need to reference the struct, it has to be registered first so we can
                ;; get its real ID... just hacking this for now with the register flag
                ;; NOTE: struct are always registered.
                (let ((struct-def (parse-struct registry struct-form)))
                  (ft-typedef-alias ptr-counter (form-def-id struct-def) 1)))

               ;; 2. Static arrays, can be see in consts, but might also be defined in struct fields
               ((list :array (and (type atom-type) atom) (and (type integer) size))
                (ft-typedef-alias ptr-counter atom size)))))
    (let ((name (get-form-name form))
          (data (match-typedef (second form) 0)))
      (if register
          (registry-add-form registry name data)
          (form-def -1 name data)))))

(defun parse-struct (registry form &key register)
  (let* ((name (get-form-name form))
         (is-id-name (typep name 'fn-id))
         (size (/ (if is-id-name (getf form :size) (third form)) 8))
         ;; TODO, so another thing is that there might be a self-ref pointer...
         ;; we need to register the struct so that fields can find it...
         ;; another ugly piece in need of improvement, basically this renders :register useless
         (forward-struct (registry-add-form registry name (ft-struct 0 '())))
         (fields
           (mapcar (lambda (f)
                     (let ((def (handler-bind
                                    ((error (lambda (c)
                                              (error "Cannot parse field definition: ~S: ~A" f c))))
                                  (parse-typedef registry (subseq f 0 2))))
                           (params (mapcar
                                    (lambda (p)
                                      (etypecase p
                                        ;; drop bit- part
                                        (keyword (intern (subseq (symbol-name p) 4) :keyword))
                                        (number (/ p 8))))
                                    (subseq f 2))))
                       (list def params)))
                   (unless (= size 0) (subseq form (if is-id-name 4 3)))))
         (data (ft-struct size fields)))
    (assert (null register) (register) "BORKEN: register can not be used with structs")
    (registry-replace-form registry (form-def-id forward-struct) data)))

(defun parse-enum (registry form &key register)
  (let* ((name (get-form-name form))
         (members (mapcar (lambda (m)
                            (list (symbol->fn-keyword (first m))
                                  (second m)))
                          (cdr form)))
         (data (ft-enum members)))
    (if register
        (registry-add-form registry name data)
        (form-def -1 name data))))

(defun parse-const (registry form &key register)
  ;; TODO: for now this only handles simple case e.g. (const :int 10) or a type alias
  ;;       no arrays, structs etc (also, structs look broken in sexp c2ffi output - no values)
  (let* ((name (get-form-name form))
         (raw-type (second form))
         (value (third form))
         (data (ft-const (if (keywordp raw-type)
                             raw-type
                             (registry-lookup-form-id registry
                                                      'ft-typedef-alias
                                                      (symbol->form-name raw-type)))
                         value)))
    (assert (symbolp raw-type) () "Expected const type to be a symbol, got ~S in ~S." raw-type form)
    (if register
        (registry-add-form registry name data)
        (form-def -1 name data))))

(defun parse-function (registry form &key register)
  (let* ((name (fn-string (first form)))
         (args (mapcar (lambda (a)
                         (handler-bind
                             ((error
                                (lambda (c)
                                  (error "Cannot parse function argument ~S: ~A" a c))))
                           (parse-typedef registry a)))
                       (second form)))
         ;; 'return-type (form name) is here just to abuse parse-typedef to get the return type
         (ret-type-form (list (make-symbol "return-type") (third form)))
         (ret-type (handler-bind
                       ((error
                          (lambda (c)
                            (error "Cannot parse function return type ~S: ~A" ret-type-form c))))
                     (parse-typedef registry ret-type-form)))
         (data (ft-function ret-type args)))
    (if register
        (registry-add-form registry name data)
        (form-def -1 name data))))

(defun parse-form (registry form &key register)
  (let ((kind (symbol-name (first form)))
        (def (cdr form)))
    (eswitch (kind :test #'string-equal)
      ("typedef" (parse-typedef registry def :register register))
      ("struct" (parse-struct registry def))
      ("enum" (parse-enum registry def :register register))
      ("const" (parse-const registry def :register register))
      ("function" (parse-function registry def :register register)))))
