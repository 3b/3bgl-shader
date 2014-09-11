(in-package #:3bgl-shaders)

(defvar *inference-worklist*)
(defvar *current-function-return-type*)
(defvar *current-function-stages*)
(defvar *current-function-local-types*)
(defclass any-type ()
  ((constraints :initform (make-hash-table) :accessor constraints :initarg :constraints)))

(defclass inference-call-site ()
  ;; can't use called function directly, since we might have multiple
  ;; calls with incompatible types
  ((called-function :accessor called-function :initarg :called-function)))

(define-condition inference-failure () ())
(define-condition incomplete-dependent (inference-failure) ())

(defclass optional-arg-type ()
  ;; used to indicate optional args, replaced by actual type
  ;; when actual number of args passed is known
  ((arg-type :initarg :arg-type :accessor arg-type)))

(defclass constrained-type ()
  ;; constraints shouldn't include equality constraints, they get
  ;; handled in UNIFY
  ((types :accessor types :initarg :types
          :initform (make-hash-table))
   (constraints :initform (make-hash-table) :accessor constraints :initarg :constraints)))

(defmethod types ((type concrete-type))
  (alexandria:alist-hash-table (list (cons type t))))

(defclass ref-type ()
  ;; instead of trying to replace type objects in constraints etc when
  ;; unifying, we just change-class to a ref-type, and the container
  ;; can resolve it to the real type on use
  ((equiv :initarg :equiv :accessor equiv :initform nil)))

(defmethod get-equiv-type (type)
  ;; possibly should error on things that aren't known types?
  type)

(defmethod get-equiv-type ((type ref-type))
  (let ((e (equiv type)))
    (assert (and e (not (eql e type))))
    (setf (equiv type) (get-equiv-type e))))


(defun debug-type-names (type)
  (typecase type
    (null nil)
    (list (mapcar 'debug-type-names type))
    (generic-type (name type))
    (any-type T)
    (constrained-type
     (debug-type-names
      (mapcar 'car
              (remove nil (alexandria:hash-table-alist
                           (types type))
                      :key 'cdr))))
    (ref-type
     (format nil "->~a" (get-equiv-type type)))
    (optional-arg-type
     (format nil "(or NIL ~s)" (debug-type-names (arg-type type))))
    (hash-table
     (let ((x))
       (maphash (lambda (k v) (when v (push (debug-type-names k) x))) type)
       x))
    (t (error "foo!"))))


(defun print-bindings/ret (name bindings ret)
  (when name (format t "inferred ~s:~%" name))
  (loop for i in bindings
        do (format t "  ~a ~@[(~s) ~]=~%       ~s~%" (if (typep i 'generic-type)
                                                         (name i)
                                                         i)
                   (if (typep i 'binding)
                       (value-type i)
                       nil)
                   (debug-type-names (if (typep i 'binding)
                                         (value-type i)
                                         i))))
  (format t "  -> ~s~%" (debug-type-names ret)))

(defmethod add-constraint (ctype constraint)
  ;; not an error, so we don't have to check before assigning constraints...
  (when *verbose* (format t "added constraint to type ~s?~%" ctype)))
(defmethod add-constraint ((ctype constrained-type) constraint)
  (setf (gethash constraint (constraints ctype)) t))
(defmethod add-constraint ((ctype any-type) constraint)
  (setf (gethash constraint (constraints ctype)) t))
(defmethod add-constraint ((ctype optional-arg-type) constraint)
  (add-constraint (arg-type ctype) constraint))
(defmethod add-constraint ((ctype ref-type) constraint)
  (add-constraint (equiv ctype) constraint))

(defclass constraint ()
  ((modified :initform nil :accessor modified)))

(defun flag-modified-constraint (constraint)
  (unless (modified constraint)
    (when *verbose* (format t "~&flag constraint ~s ~s~%" constraint (name constraint)))
    (setf (modified constraint) t)
    (push constraint *inference-worklist*)))

(defun flag-modified-type (type)
  (setf type (get-equiv-type type))
  (when *verbose*
    (format t "~&flag type ~s (~s)~%" type (debug-type-names type)))
  (maphash (lambda (constraint v)
             (when v (flag-modified-constraint constraint)))
           (constraints type)))

(defclass function-application (constraint)
  ((argument-types :accessor argument-types :initform nil)
   (return-type :initarg :return-type :accessor return-type) ;; SET-TYPE?
   (function-types :initarg :function-types :accessor function-types) ;; modifiable list of ftype, each is a list of concrete arg types + concrete ret type
   (name :initarg :name :accessor name :initform nil))) ;; for debugging

;;; used to store types for variable arity built-in functions (or ones with
;;; optional args)
;;; converted to a normal function-application with specific arity
;;; before type inference, when we know how many arguments are passed
;;; to it
(defclass variable-arity-function-application (function-application)
  ((min-arity :initarg :min-arity :accessor min-arity :initform 0)
   (max-arity :initarg :max-arity :accessor max-arity :initform 0)
   ;; array of MAX-ARITY+1 lists of ftypes
   ;;   entry N contains ftypes of arity N, or NIL if function doesn't
   ;;   accept that arity
   ;; ftype is list of list of concrete arg types + concrete return type
   ;;   ex ((int int int) vec3)
   (function-types-by-arity :initarg :function-types-by-arity
                            :accessor function-types-by-arity)))

(defclass global-function-constraint (constraint)
  ;; doesn't actually constrain things, just a place to track changes
  ;; to argument bindings' types
  ((name :accessor name :initarg :name :initform nil)
   (function :accessor global-function :initarg :function)
   (argument-types :accessor argument-types :initform nil)
   (return-type :initarg :return-type :accessor return-type)))

(defclass cast-constraint (constraint)
  ;; represents a possible cast from IN-TYPE to OUT-TYPE
  ;; (can't just unify them, since we might have other incompatible
  ;; uses for IN. for example in (setf a 1) (setf b a) (* a uvec)
  ;; (setf b 1.2), A needs to be :INT or :UINT for the multiplication
  ;; and B needs to be :FLOAT for the 2nd assignment, so they can't be
  ;; unified directly at (SETF B A) it needs to unify something like
  ;; (SETF B (CAST A)) where knowing B is a float means A is
  ;; INT/UINT/FLOAT, and knowing A is INT/UINT means B is
  ;; INT/UINT/FLOAT
  ;;
  ;; on update, if none of the input or output types can be involved
  ;; in an implicit cast (like structs or samplers), it should remove
  ;; itself from in/out and unify them normally
  ;;
  ;; cast-type can be :implicit or :explicit, which use corresponding
  ;; slots in the types
  ((cast-type :initform :implicit :accessor cast-type :initarg :cast-type)
   (in-type :initarg :in :accessor in-type)
   (out-type :initarg :out :accessor out-type)
   (name :initarg :name :accessor name :initform nil)))

(defclass ctype/other-constraint (constraint)
  ((ctype :initarg :ctype :accessor ctype)
   (other-type :initarg :other-type :accessor other-type)))

(defclass same-size-different-base-type-constraint (ctype/other-constraint)
  ;; CTYPE is restricted to a vector or scalar with same number of
  ;; elements as OTHER-TYPE, but with elements of BASE-TYPE
  ;; ex base-type :bool => :float -> bool, :vec2 -> :bvec2, :ivec3 ->  :bvec3
  ((base-type :initarg :base-type :accessor base-type)))

(defclass same-base-type-different-size-constraint (ctype/other-constraint)
  ;; CTYPE is restricted to a vector or scalar with fixed size
  ;; but same base-type as OTHER-TYPE, which must be a vector with
  ;; at least MIN-SIZE elements
  ;; ex (or :vec3 :ivec4) -> (or :vec2 :ivec2)
  ((out-size :initarg :out-size :accessor out-size)
   (min-size :initarg :min-size :accessor min-size)))

(defclass same-type-or-scalar-constraint (ctype/other-constraint)
  ;; CTYPE is same type as OTHER-TYPE or a scalar of same type as base
  ;; type of OTHER-TYPE
  ;; ex :float -> :float, :ivec2 -> (or :ivec2 :int), :vec3 -> (or :vec3 :float)
  ())

(defclass scalar-type-of-constraint (ctype/other-constraint)
  ;; CTYPE is scalar base type of OTHER-TYPE
  ;; ex :float -> :float, :ivec2 -> :int, :vec3 -> :float
  ())

(defclass array-access-constraint (ctype/other-constraint)
  ;; CTYPE is array type of length at least MIN-SIZE containing
  ;; elements of type OTHER-TYPE
  ((min-size :initarg :min-size :accessor min-size)))

(defmethod dump-constraint ((c cast-constraint))
  (format t "~& cast ~s @ ~s:~%   ~s~%-> ~s~%" c (name c)
          (debug-type-names (in-type c))
          (debug-type-names (out-type c))))

(defmethod dump-constraint ((c variable-arity-function-application))
  (format t "~& variable arity ~s ~s/~s~%" c
          (min-arity c) (max-arity c)))

(defmethod dump-constraint ((c function-application))
  (format t "~& function application ~s ~s:~%   ~s~%-> ~s~%" c (name c)
          (debug-type-names (argument-types c))
          (debug-type-names (return-type c))))

(defmethod dump-constraint ((c global-function-constraint))
  (format t "~& g-f-c ~s ~s/~s:~%  ~s~%" c (name c)
          (name (global-function c))
          (argument-types c)))

(defmethod dump-constraint ((c ctype/other-constraint))
  (format t "  ~s~%  ~s~%"
          (debug-type-names (ctype c))
          (debug-type-names (other-type c))))
(defmethod dump-constraint ((c same-base-type-different-size-constraint))
  (format t "~& same-base/diff-size ~s~%  ~s -> ~s" c
          (min-size c) (out-size c))
  (call-next-method))
(defmethod dump-constraint ((c same-type-or-scalar-constraint))
  (format t "~& same or scalar ~s~%" c)
  (call-next-method))
(defmethod dump-constraint ((c scalar-type-of-constraint))
  (format t "~& ->scalar ~s~%" c)
  (call-next-method))
(defmethod dump-constraint ((c array-access-constraint))
  (format t "~& array-access ~s [~s]~%" c (min-size c))
  (call-next-method))


(defparameter *copy-constraints-hash* nil
  "used to track already copied constraints when copying type inference data")


(defmacro defmethod2 (name (a b) &body body)
  ;; define methods on NAME with arguments A B and B A
  `(progn
     (defmethod ,name (,a ,b) ,@body)
     (defmethod ,name (,b ,a) ,@body)))


;; unify types A, B, updating any constraints if needed, and return unified type
;; (should UNIFY handle any implicit cast stuff? assuming not for now,
;;  possibly will want a separate 'concrete' type for casts though?
(defmethod unify (a b)
  (unless (eq a b)
    (error "can't unify ~s and ~s yet!" a b))
  a)

(defmethod unify ((a any-type) (b any-type))
  (unless (eq a b)
    ;; move all of B's constraints to A, and make B a ref to A
    (loop for c being the hash-keys of (constraints b) using (hash-value v)
          when v
            ;; don't need to flag constraints, since neither A or B
            ;; provide any new info
            do (setf (gethash c (constraints a)) t))
    (change-class b 'ref-type :equiv a))
  a)

(defmethod unify ((a constrained-type) (b constrained-type))
  ;; merge sets of types
  ;; if only 1 type left
  ;;   make A and B refs to a concrete type
  ;;   flag all constraints
  ;;      (for now assuming A and B had more than 1 valid type(
  ;;   return concrete type
  ;; else
  ;;   flag constraints if set of types changed
  ;;   move constraints from B to A
  ;;   make B a ref to A
  ;;   return A

  (let ((types (make-hash-table))
        (last-type nil))
    (maphash (lambda (k v) (when (and v (gethash k (types a)))
                             (setf last-type k)
                             (setf (gethash k types) t)))
             (types b))
    (case (hash-table-count types)
      (0 (error "failed to unify types ~s, ~s" a b))
      (1
       ;; LAST-TYPE = single type
       (loop for c being the hash-keys of (constraints a) using (hash-value v)
             when v
               do (flag-modified-constraint c))
       (loop for c being the hash-keys of (constraints b) using (hash-value v)
             when v
               do (flag-modified-constraint c))
       (change-class a 'ref-type :equiv last-type)
       (change-class b 'ref-type :equiv last-type)
       last-type)
      (t
       ;; multiple types
       (when (/= (hash-table-count types)
                 (hash-table-count (types a)))
         (loop for c being the hash-keys of (constraints a) using (hash-value v)
               when v
                 do (flag-modified-constraint c)))
       (setf (types a) types)
       (loop with flag = (/= (hash-table-count types)
                             (hash-table-count (types b)))
             for c being the hash-keys of (constraints b) using (hash-value v)
             when v
               do (when flag
                    (flag-modified-constraint c))
                  (setf (gethash c (constraints a)) t))
       (change-class b 'ref-type :equiv a)
       a))))

(defmethod2 unify ((a constrained-type) (b generic-type))
  ;; make sure B is in A's set of valid types
  ;; remove A,B from constraints, add B
  ;; flag constraint modified
  ;; return B
  (error "constrained x generic not done yet"))

(defmethod2 unify ((a constrained-type) (b concrete-type))
  ;; if B is valid type for A, make A a ref to B, and flag constraints
  ;; (for now, assuming constrainted types have multiple types)
  (assert (gethash b (types a)))
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        when v
          do (flag-modified-constraint c))
  (change-class a 'ref-type :equiv b)
  b)

(defmethod2 unify ((a any-type) (b concrete-type))
  ;; same as constrained-type/concrete-type, except all types are valid
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        when v
          do (flag-modified-constraint c))
  (change-class a 'ref-type :equiv b)
  b)

(defmethod2 unify ((a constrained-type) (b any-type))
  ;; add B's constraints to A, flag them modified, make B a ref to A
  (loop for c being the hash-keys of (constraints b) using (hash-value v)
        when v
          do (flag-modified-constraint c)
             (setf (gethash c (constraints a)) t))
  (change-class b 'ref-type :equiv a)
  a)

(defmethod2 unify ((a constrained-type) (b null))
  ;; NIL is alias for :void concrete type
  (let ((void (get-type-binding :void)))
    (assert (or (gethash b (types a))
                (gethash void (types a))))
    (loop for c being the hash-keys of (constraints a) using (hash-value v)
          when v
            do (flag-modified-constraint c))
    (change-class a 'ref-type :equiv void)
    void))

(defmethod2 unify ((a ref-type) b)
  (setf (equiv a) (unify (equiv a) b)))



(defclass infer-build-constraints (glsl::glsl-walker)
  ())

(defun set-type (types &key constraint)
  (etypecase types
    ((eql t)
     (make-instance 'any-type
                    :constraints (alexandria:plist-hash-table
                                  (when constraint (list constraint t)))))
    (symbol
     (or (get-type-binding types)
         (error "unknown type ~s?" types)))
    (concrete-type
     types)
    (list
     (let ((set (make-hash-table)))
       (loop for i in types
             when (symbolp i)
               do (setf i (or (get-type-binding i)
                              (error "unknown type ~s?" types)))
             do (setf (gethash i set) t))
       (make-instance 'constrained-type
                      :types set :constraints (alexandria:plist-hash-table
                                               (when constraint
                                                 (list constraint t))))))))

(defmethod copy-constraints :around (x)
  ;; we might have NIL as cached value, so check 2nd value of gethash
  (multiple-value-bind (cached found)
      (gethash (get-equiv-type x) *copy-constraints-hash*)
    (if found
        cached
        (call-next-method))))

(defmethod copy-constraints ((constraint function-application))
  (let ((copy (make-instance 'function-application
                             :name (name constraint)
                             :function-types (copy-list (function-types
                                                         constraint)))))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'argument-types)
          (mapcar 'copy-constraints (argument-types constraint)))
    (setf (slot-value copy 'return-type)
          (copy-constraints (return-type constraint)))
    copy))

(defmethod copy-constraints ((constraint variable-arity-function-application))
  (let ((copy (make-instance 'variable-arity-function-application
                             :name (name constraint)
                             :function-types-by-arity
                             (map 'vector #'copy-list
                                  (function-types-by-arity constraint)))))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'return-type)
          (copy-constraints (return-type constraint)))
    (setf (slot-value copy 'argument-types)
          (mapcar 'copy-constraints (argument-types constraint)))
    copy))


(defmethod copy-constraints ((constraint global-function-constraint))
  (let ((copy (make-instance 'global-function-constraint
                             :name (name constraint)
                             :function (global-function constraint))))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'argument-types)
          (mapcar 'copy-constraints (argument-types constraint)))
    (setf (slot-value copy 'return-type)
          (copy-constraints (return-type constraint)))
    copy))

(defmethod copy-constraints ((constraint same-type-or-scalar-constraint))
  (let ((copy (make-instance 'same-type-or-scalar-constraint)))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'ctype)
          (copy-constraints (ctype constraint)))
    (setf (slot-value copy 'other-type)
          (copy-constraints (other-type constraint)))
    copy))

(defmethod copy-constraints ((constraint scalar-type-of-constraint))
  (let ((copy (make-instance 'scalar-type-of-constraint)))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'ctype)
          (copy-constraints (ctype constraint)))
    (setf (slot-value copy 'other-type)
          (copy-constraints (other-type constraint)))
    copy))

(defmethod copy-constraints ((constraint cast-constraint))
  (let ((copy (make-instance 'cast-constraint
                             :name (name constraint)
                             :cast-type (cast-type constraint))))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'in-type)
          (copy-constraints (in-type constraint)))
    (setf (slot-value copy 'out-type)
          (copy-constraints (out-type constraint)))
    copy))

(defmethod copy-constraints ((constraint same-size-different-base-type-constraint))
  (let ((copy (make-instance 'same-size-different-base-type-constraint)))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    ;; not sure if we need to actually copy base type, it should be concrete?
    ;; (probably should enforce that before skipping copy though)
    (setf (slot-value copy 'base-type)
          (copy-constraints (base-type constraint)))
    (setf (slot-value copy 'ctype)
          (copy-constraints (ctype constraint)))
    (setf (slot-value copy 'other-type)
          (copy-constraints (other-type constraint)))
    copy))

(defmethod copy-constraints ((constraint same-base-type-different-size-constraint))
  (let ((copy (make-instance 'same-base-type-different-size-constraint)))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (setf (slot-value copy 'min-size) (min-size constraint))
    (setf (slot-value copy 'out-size) (out-size constraint))
    (setf (slot-value copy 'ctype)
          (copy-constraints (ctype constraint)))
    (setf (slot-value copy 'other-type)
          (copy-constraints (other-type constraint)))
    copy))

(defmethod copy-constraints ((type generic-type))
  ;; concrete types and aggregates don't need copied
  (let ((e (or (get-equiv-type type) type)))
    (setf (gethash type *copy-constraints-hash*) e
          (gethash e *copy-constraints-hash*) e)))

(defmethod copy-constraints ((type null))
  ;; doesn't need copied
  (setf (gethash type *copy-constraints-hash*) type))

(defmethod copy-constraints ((hash hash-table))
  (let ((n (make-hash-table)))
    (maphash (lambda (k v)
               (when v
                 (setf (gethash (copy-constraints k) n) t)))
             hash)
    n))

(defmethod copy-constraints ((type ref-type))
  (copy-constraints (get-equiv-type type)))

(defmethod copy-constraints ((type constrained-type))
  (let* ((copy (make-instance 'constrained-type)))
    ;; we need to update cache before copying constraints because they
    ;; link back to type
    (setf (gethash type *copy-constraints-hash*) copy)
    (setf (slot-value copy 'constraints)
          (copy-constraints (constraints type)))
    (setf (slot-value copy 'types)
          (copy-constraints (types type)))
    copy))

(defun expand-optional-arg-type (o-a-t)
  (if (typep o-a-t 'optional-arg-type)
      (change-class o-a-t 'ref-type :equiv (arg-type o-a-t))
      o-a-t))

(defmethod copy-constraints ((type optional-arg-type))
  (let* ((copy (make-instance 'optional-arg-type)))
    ;; we need to update cache before copying constraints because they
    ;; link back to type
    (setf (gethash type *copy-constraints-hash*) copy)
    (setf (slot-value copy 'arg-type)
          (copy-constraints (arg-type type)))
    (setf (gethash type *copy-constraints-hash*)
          (expand-optional-arg-type copy))))

(defmethod copy-constraints ((type any-type))
  (let* ((copy (make-instance 'any-type)))
    ;; we need to update cache before copying constraints because they
    ;; link back to type
    (setf (gethash type *copy-constraints-hash*) copy)
    (setf (slot-value copy 'constraints)
          (copy-constraints (constraints type)))
    copy))


(defun copy-unify-constraints (type unify-type &key cast name)
  (unless unify-type
    (assert (typep type 'optional-arg-type))
    (return-from copy-unify-constraints nil))
  (let ((copy (copy-constraints type)))
    (if cast
        (let ((cast-constraint (make-instance 'cast-constraint
                                              :name name
                                              :in unify-type
                                              :out copy
                                              :cast-type cast)))
          (assert unify-type)
          (add-constraint copy cast-constraint)
          (add-constraint unify-type cast-constraint)
          (flag-modified-constraint cast-constraint)
          copy)
        (unify copy unify-type))))

(defmethod walk (form (walker infer-build-constraints))
  (break "walk" form)
  (call-next-method))

(defun cast-to-boolean (type)
  ;; no implicit casts to bool, so just force type to be bool...
  (let ((bool (get-type-binding :bool)))
    (etypecase type
      (any-type
       (flag-modified-type type)
       (change-class type 'ref-type :equiv bool)
       bool)
      (constrained-type
       (assert (gethash bool (types type)))
       (unless (= 1 (hash-table-count (types type)))
         (flag-modified-type type))
       (change-class type 'ref-type :equiv bool)
       bool)
      (ref-type
       (setf (equiv type) (cast-to-boolean (equiv type))))
      (concrete-type
       (assert (eq type bool))))))

(defmethod walk ((form if-form) (walker infer-build-constraints))
  ;; todo: unify test-form with types accepted by IF? (scalars? booleans?)
  (cast-to-boolean (walk (test-form form) walker))
  (if (and (then-form form) (else-form form))
      (unify (walk (then-form form) walker)
             (walk (else-form form) walker))
      (if (then-form form)
          (walk (then-form form) walker)
          (walk (else-form form) walker))))

(defmethod walk ((form for-loop) (walker infer-build-constraints))
  ;; walk init/step forms, ignore ret?
  ;; walk condition-forms, casts to boolean(or scalar?)?
  ;; walk body (ignore ret?)
  (loop for a in (init-forms form) do (walk a walker))
  (loop for a in (step-forms form) do (walk a walker))
  (loop for a in (condition-forms form)
        for ret = (walk a walker)
        finally (cast-to-boolean ret))
  ;; for now ignoring return type, since 'for' is a statement in glsl
  (loop for a in (body form) do (walk a walker)))

(defmethod walk ((form swizzle-access) (walker infer-build-constraints))
  (let* ((binding-type (walk (binding form) walker))
         (n (length (field form)))
         ;; fixme: don't hard code this?
         (vector-types #(()
                         (:bool :int :uint :float :double)
                         (:bvec2 :ivec2 :uvec2 :vec2 :dvec2)
                         (:bvec3 :ivec3 :uvec3 :vec3 :dvec3)
                         (:bvec4 :ivec4 :uvec4 :vec4 :dvec4)))
         (out-type (set-type (aref vector-types n)))
         (valid-vector-types (loop for i from (max 2 (1+ (min-size form)))
                                     below 5
                                   append (aref vector-types i))))
    ;; not sure yet if this should be possible or not...wrap with
    ;; get-equiv-type if so
    (assert (not (typep binding-type 'ref-type)))
    (setf (value-type form)
          (cond
            ;; check for known output type
            ((typep binding-type 'concrete-type)
             (aref (scalar/vector-set binding-type) n))
            ((and (typep binding-type 'constrained-type)
                  (= 1 (hash-table-count (types binding-type))))
             (error "shouldn't happen anymore?"))
            (t
             (let ((c (make-instance 'same-base-type-different-size-constraint
                                     :out-size n
                                     :min-size (1+ (min-size form))
                                     :ctype out-type
                                     :other-type binding-type)))
               (cond
                 ((typep binding-type 'any-type)
                  (change-class binding-type 'constrained-type
                                :types (alexandria:alist-hash-table
                                        (mapcar (lambda (a)
                                                  (cons (get-type-binding a) t))
                                                valid-vector-types))))
                 ((typep binding-type 'constrained-type)
                  (setf binding-type
                        (unify binding-type (set-type valid-vector-types)))))
               (add-constraint binding-type c)
               (add-constraint out-type c)
               (flag-modified-constraint c)
               out-type))))))

(defmethod walk ((form array-access) (walker infer-build-constraints))
  (let ((binding-type (walk (binding form) walker)))
    ;; just returning array type for now...
    ;; fixme: add constraints on size
    (value-type binding-type)))


(defmethod walk ((form slot-access) (walker infer-build-constraints))
  ;; todo: for now requiring structs to have declared type, but eventually
  ;; should check type of binding, and add constraint that it be a struct
  ;; with specified field (which will require tracking known structs, etc)
  (let ((struct-type (walk (binding form) walker)))
    ;; fixme: should structs have a hash for O(1) access?
    ;; or should something earlier have already looked up the specific slot?
    (loop for binding in (bindings struct-type)
          when (or (eql (name binding) (field form))
                   ;; todo: decide if (.foo struct) accessor should be
                   ;; kept?  if so, should it intern symbols instead
                   ;; of doing string compare?
                   (equal (string (name binding)) (field form)))
            do (return-from walk (value-type binding)))
    (break "slot-access" (list form struct-type))))

(defmethod walk ((form function-call) (walker infer-build-constraints))
  (let* ((called (called-function form))
         (*copy-constraints-hash* (make-hash-table))
         (call-site (when (typep called 'global-function)
                      (make-instance 'inference-call-site
                                     :called-function called))))
    (when (typep called 'unknown-function-binding)
      (error "got call to unknown function ~s during type inference"
             (name called)))
    (unless (eq t (type-inference-state called))
      (format t "got call to function ~s with incomplete or failed type inference ~s?" (name called) (type-inference-state called))
      (error 'incomplete-dependent))
    ;; make local copies of any types/constraints affected by function
    ;; args, and unify with actual arg types
    ;; (unify will add modified constraints to work list)
    (assert (<= (length (arguments form))
                (length (bindings called))))
    ;; store NIL in the cache for any unused args, so we don't try to copy them
    ;; while walking other args
    (loop for binding in (nthcdr (length (arguments form)) (bindings called))
          do (setf (gethash (value-type binding) *copy-constraints-hash*) nil))

    ;; walk remaining args and copy as usual
    (let ((a (loop with args = (arguments form)
                   for arg = (pop args)
                   for binding in (bindings called)
                   collect (copy-unify-constraints
                            (value-type binding)
                            (if arg
                                (walk arg walker)
                                nil)
                            :cast (and arg (allow-casts binding))
                            :name (name binding)))))
      (when call-site
        (setf (gethash call-site *current-function-local-types*) a)))
    ;; copy return type and any linked constraints
    ;; (may have already been copied if it depends on arguments)
    (if (eq (called-function form)
            (get-function-binding 'return
                                  :env glsl::*glsl-base-environment*))
        ;; if we are calling RETURN, unify return type with function
        ;; return as well
        (let* ((r (copy-constraints (value-type called))))
          (if (boundp '*current-function-return-type*)
              (setf *current-function-return-type*
                    ;; don't need a cast, since input to RETURN already has one
                    (unify r *current-function-return-type*))
              (setf *current-function-return-type* r)))
        ;; normal function, just copy the return values as usual
        (let ((vt (copy-constraints (value-type called))))
          (when call-site
            (push vt (gethash call-site *current-function-local-types*)))
          vt))))

(defmethod walk ((form variable-read) (walker infer-build-constraints))
  (walk (binding form) walker))

(defmethod walk ((form variable-write) (walker infer-build-constraints))
  (let* ((binding (walk (binding form) walker))
         (value (walk (value form) walker))
         ;; todo: avoid creating cast constraint if we know both types?
         (cast (make-instance 'cast-constraint
                              :name form
                              :in value
                              :out binding)))
    (assert value)
    (add-constraint binding cast)
    (add-constraint value cast)
    (flag-modified-constraint cast)
    value))

(defmethod walk ((form local-variable) (walker infer-build-constraints))
  (value-type form))

(defmethod walk ((form binding) (walker infer-build-constraints))
  (value-type form))

(defmethod walk ((form interface-binding) (walker infer-build-constraints))
  (let ((stage-binding (stage-binding form)))
    (assert stage-binding)
    (walk stage-binding walker)))

(defmethod walk ((form interface-stage-binding) (walker infer-build-constraints))
  (walk (binding form) walker))

(defmethod walk ((form constant-binding) (walker infer-build-constraints))
  (if (member (value-type form) '(t nil))
      (walk (initial-value-form form) walker)
      (value-type form)))

(defmethod walk ((form function-argument) (walker infer-build-constraints))
  (value-type form))

(defmethod walk ((form integer) (walker infer-build-constraints))
  (set-type (list :int)))

(defmethod walk ((form float) (walker infer-build-constraints))
  (set-type :float))

(defmethod walk ((form explicit-progn) (walker infer-build-constraints))
  (loop for a in (body form)
        for ret = (walk a walker)
        finally (return ret)))

(defmethod walk ((form binding-scope) (walker infer-build-constraints))
  (loop for binding in (bindings form)
        for declared-type = (if (eq t (declared-type binding))
                                (make-instance 'any-type)
                                (set-type (declared-type binding)))
        for initial-value-type = (walk (initial-value-form binding) walker)
        when initial-value-type
          do (let ((cast (make-instance 'cast-constraint
                                        :name (name
                                               (initial-value-form
                                                binding))
                                        :cast-type :implicit
                                        :in initial-value-type
                                        :out declared-type)))
               (add-constraint declared-type cast)
               (add-constraint initial-value-type cast)
               (flag-modified-constraint cast))
        collect (setf (value-type binding) declared-type)
        do (setf (gethash binding *current-function-local-types*)
                 (value-type binding)))
  (loop for a in (body form)
        for ret = (walk a walker)
        finally (return ret)))

(defmethod walk ((form global-function) (walker infer-build-constraints))
  (let* ((*current-function-return-type* (make-instance 'any-type))
         (*current-function-stages* (list t)))
    (loop for binding in (bindings form)
          for declared-type = (declared-type binding)
          ;; fixme: is this still right/useful?
          if (typep declared-type '(cons (eql T) (cons unsigned-byte)))
            collect (setf (value-type binding)
                          (value-type (elt (bindings form)
                                           (second declared-type))))
          else collect (setf (value-type binding)
                             (set-type declared-type))
          do (setf (gethash binding *current-function-local-types*)
                   (value-type binding)))
    ;; fixme: add a constraint (or just a type?) if return type isn't T
    (when (and (declared-type form)
               (not (eq (declared-type form) t)))
      (when (or (not (value-type form))
                (eq t (value-type form)))
        (setf (value-type form) (declared-type form)))
      (when (typep (value-type form) 'any-type)
        (setf (value-type form) (unify (value-type form)
                                       (declared-type form)))))

    (loop for a in (body form)
          do (walk a walker))
    (if (and (value-type form)
             (not (eq (value-type form) t)))
        (setf (gethash :return *current-function-local-types*)
              (unify (value-type form) *current-function-return-type*))
        (setf (gethash :return *current-function-local-types*)
              *current-function-return-type*
              (value-type form)
              *current-function-return-type*))))


(defmethod walk (form (walker infer-build-constraints))
  (when *verbose* (format t "unhandled form ~s~%" form))
  (when (next-method-p)
    (call-next-method)))

(defmethod unifiable-types-p (x type)
  (error "can't tell if ~s unifies with ~s?" x type))

(defmethod unifiable-types-p ((a null) (b null))
  ;; is this right?
  t)

(defmethod2 unifiable-types-p ((a null) (b any-type))
  nil)

(defmethod2 unifiable-types-p ((a null) (b generic-type))
  nil)

(defmethod2 is-type ((a null) b)
  nil)

(defmethod2 unifiable-types-p ((a any-type) b)
  t)

(defmethod2 unifiable-types-p ((a generic-type) b)
  (eq a b))

(defmethod2 unifiable-types-p ((a generic-type) (b generic-type))
  (eq a b))

(defmethod2 unifiable-types-p ((a generic-type) (b constrained-type))
  (gethash a (types b)))

(defmethod2 unifiable-types-p ((a ref-type) b)
  (unifiable-types-p (get-equiv-type a) b))

(defmethod unifiable-types-p ((a ref-type) (b ref-type))
  (unifiable-types-p (get-equiv-type a) (get-equiv-type b)))

(defmethod unifiable-types-p ((x constrained-type) (type constrained-type))
  (if (< (hash-table-count (types type))
         (hash-table-count (types x)))
      (rotatef x type))
  (loop for k being the hash-keys of (types x) using (hash-value v)
          thereis (and v (gethash k (types type)))))

(defmethod2 unifiable-types-p ((a symbol) b)
  (let ((at (get-type-binding a)))
    (when at
      (unifiable-types-p at b))))

(defmethod update-constraint ((constraint variable-arity-function-application))
  ;; figure out how many actual arguments we have, and convert to a normal
  ;; function-application with that arity
  ;; (or error if we don't have any matching ftypes)
  (let ((arity (or (position nil (argument-types constraint))
                   (length (argument-types constraint)))))
    (assert (and (array-in-bounds-p (function-types-by-arity constraint) arity)
                 (plusp
                  (length (aref (function-types-by-arity constraint) arity)))))
    (setf (function-types constraint)
          (aref (function-types-by-arity constraint) arity))
    (change-class constraint 'function-application)
    (update-constraint constraint)))

(defmethod flatten-types ((types ref-type))
  (get-equiv-type types))

(defmethod flatten-types ((types any-type))
  types)

(defmethod flatten-types ((types constrained-type))
  types)

(defmethod flatten-types ((types cons))
  (mapcar 'get-equiv-type types))

(defmethod flatten-types ((types hash-table))
  (let ((h (make-hash-table)))
    (maphash (lambda (k v)
               (when v
                 (setf (gethash (get-equiv-type k) h) t)))
             types)
    h))

(defmethod flatten-types ((types ctype/other-constraint))
  (setf (ctype types) (flatten-types (ctype types)))
  (setf (other-type types) (flatten-types (other-type types))))

(defmethod update-constraint ((constraint function-application))
  ;; fixme: keep track of which args were modified and only process them...
  ;; fixme: rearrange data structures to avoid copies here?
  (let ((removed 0)
        (removed2 0)
        (arg-type-counts (make-array (length (argument-types constraint))
                                     :initial-element 0))
        (return-type-count t))
    ;; no point in processing a T*->T function, since it won't
    ;; restrict any of the arguments or return type, so it shouldn't
    ;; have a constraint in the first place...
    (assert (not (eql t (function-types constraint))))
    ;; get rid of any ref-types
    (setf (argument-types constraint)
          (flatten-types (argument-types constraint)))
    (setf (return-type constraint)
          (get-equiv-type (return-type constraint)))
    ;; count possible types so we can tell if they changed
    (loop for a in (argument-types constraint)
          for i from 0
          when (typep a 'constrained-type)
            do (setf (aref arg-type-counts i) (hash-table-count (types a))))
    (typecase (return-type constraint)
      (constrained-type
       (setf return-type-count
             (hash-table-count (types (return-type constraint)))))
      (concrete-type
       (setf return-type-count 1)))

    ;; loop through function types, keep ones that match the argument/ret types
    (setf (function-types constraint)
          (loop for ftype in (function-types constraint)
                for (args ret) = ftype
                when (and
                      ;; fixme: store concrete types in ftypes instead of symbols
                      (unifiable-types-p ret (return-type constraint))
                      (loop for .ftype-arg = (pop args)
                            for ftype-arg = (or (get-type-binding .ftype-arg)
                                                .ftype-arg)
                            for arg in (argument-types constraint)
                            always (unifiable-types-p arg ftype-arg)))
                  collect ftype
                else do (incf removed)))

    ;; loop through argument types, set all to NIL
    (flet ((process (arg)
             (unless (or (not arg)
                         (typep arg 'any-type)
                         (typep arg 'concrete-type))
               (maphash (lambda (k v)
                          (declare (ignore v))
                          (setf (gethash k (types arg)) nil))
                        (types arg)))))
      (mapcar #'process (argument-types constraint))
      (process (return-type constraint)))
    ;; expand any-type args/ret
    ;; fixme: does this code ever get used?
    (flet ((hash-list (l)
             (let ((h (make-hash-table)))
               (loop for i in l do (setf (gethash i h) nil))
               h)))
      (when (typep (return-type constraint) 'any-type)
        (break "any type?")
        (change-class (return-type constraint)
                      'constrained-type
                      :types (hash-list (mapcar 'second
                                                (function-types constraint)))))
      (loop for arg in (argument-types constraint)
            for i from 0
            when (typep arg 'any-type)
              do (change-class arg 'constrained-type
                               :types (hash-list
                                       (mapcar (lambda (a) (nth i (first a)))
                                               (function-types constraint))))))

    ;; loop through function types again, turn back on the valid argument types
    (loop for (nil .ret) in (function-types constraint)
          for ret = (or (get-type-binding .ret) .ret)
          if (typep (return-type constraint) 'concrete-type)
            do (assert (unifiable-types-p ret (return-type constraint)))
          else
            do (setf (gethash ret (types (return-type constraint)))
                     t))
    (loop for ftype in (function-types constraint)
          for (args .ret) = ftype
          for ret = (or (get-type-binding .ret) .ret)
          if (typep (return-type constraint) 'concrete-type)
            do (assert (unifiable-types-p ret (return-type constraint)))
          else
            do (assert (nth-value 1 (gethash ret (types (return-type constraint)))))
          do (loop with arg-types = (argument-types constraint)
                   for .ftype-arg in args
                   for ftype-arg = (or (get-type-binding .ftype-arg)
                                       .ftype-arg)
                   for arg = (pop arg-types)
                   unless (typep arg 'concrete-type)
                     do (assert (nth-value 1 (gethash ftype-arg (types arg))))
                        (setf (gethash ftype-arg (types arg)) t)))
    ;; remove any unused types from the argument type hashes, flag
    ;; modified types, and replace types with only 1 type left
    (flet ((process (arg c)
             (if (or (not arg)
                     (typep arg 'any-type)
                     (typep arg 'concrete-type))
                 (progn
                   (when (plusp c)
                     (incf removed2))
                   arg)
                 (let ((last-type))
                   (maphash (lambda (k v)
                              (if v
                                  (setf last-type k)
                                  (remhash k (types arg))))
                            (types arg))
                   (unless (= c (hash-table-count (types arg)))
                     (incf removed2)
                     (flag-modified-type arg))
                   (if (= 1 (hash-table-count (types arg)))
                       (progn (change-class arg 'ref-type :equiv last-type)
                              last-type)
                       arg)))))
      (setf (argument-types constraint)
            (map 'list #'process (argument-types constraint)
                 arg-type-counts))
      (setf (return-type constraint)
            (process (return-type constraint)
                     return-type-count)))
    (unless (or (plusp removed) (plusp removed2))
      (warn "couldn't narrow constraint?"))))

(defun get-only-hash-key (hash)
  (let ((last))
    (maphash (lambda (k v)
               (assert v)
               (assert (not last))
               (setf last k))
             hash)
    last))

(defmethod update-constraint ((constraint cast-constraint))
  (setf (in-type constraint) (get-equiv-type (in-type constraint)))
  (setf (out-type constraint) (get-equiv-type (out-type constraint)))
  (let ((in-casts (make-hash-table))
        (out-casts (make-hash-table))
        (any-in (typep (in-type constraint) 'any-type))
        (any-out (typep (out-type constraint) 'any-type)))
    (labels ((add-casts (type casts hash)
               (setf (gethash type hash) t)
               (loop for cast in casts
                     do (setf (gethash cast hash) t)))
             (map-concrete-types (type fun)
               (etypecase type
                 (null)
                 (concrete-type
                  (funcall fun type))
                 (constrained-type
                  (maphash (lambda (k v) (when v (map-concrete-types k fun)))
                           (types type)))
                 (optional-arg-type
                  (map-concrete-types (arg-type type) fun))))
             (handle-fixed-constraint ()
               (when (or (and (not any-in)
                              (zerop (hash-table-count in-casts)))
                         (and (not any-out)
                              (zerop (hash-table-count out-casts))))
                 (error "can't resolve constraint?"))
               ;; if both types only have 1 type left, get rid of the
               ;; constraint (handled indirectly by unify, which will
               ;; also remove+flag any other constraints)
               (when (and (= 1 (hash-table-count in-casts))
                          (= 1 (hash-table-count out-casts)))
                 (setf (in-type constraint)
                       (unify (in-type constraint)
                              (get-only-hash-key in-casts)))
                 (setf (out-type constraint)
                       (unify (out-type constraint)
                              (get-only-hash-key out-casts)))
                 t)))
      (unless any-in
        (map-concrete-types (in-type constraint)
                            (lambda (x)
                              (add-casts x (if (eq :explicit
                                                   (cast-type constraint))
                                               (explicit-casts x)
                                               (implicit-casts-to x))
                                         in-casts))))
      (unless any-out
        (map-concrete-types (out-type constraint)
                            (lambda (x)
                              (add-casts x (if (eq :explicit
                                                   (cast-type constraint))
                                               (explicit-casts x)
                                               (implicit-casts-from x))
                                         out-casts))))

      (cond
        ((and any-in any-out)
         ;; do nothing, can't constrain either type...
         )
        (any-in
         (or (handle-fixed-constraint)
             (unify (in-type constraint)
                    (make-instance 'constrained-type
                                   :types out-casts))))
        (any-out
         (or (handle-fixed-constraint)
             (unify (out-type constraint)
                    (make-instance 'constrained-type
                                   :types in-casts))))
        ((or (not (in-type constraint))
             (not (out-type constraint)))
         (unify (in-type constraint)
                (out-type constraint)))
        (t
         (flet ((c (type cast-types)
                  (typecase type
                    (constrained-type
                     (let ((old (hash-table-count (types type))))
                       (maphash (lambda (k v)
                                  (unless (and v (gethash k cast-types))
                                    (remhash k (types type))))
                                (types type))
                       (assert (plusp (hash-table-count (types type))))
                       (when (/= old (hash-table-count (types type)))
                         (flag-modified-type type))))
                    (concrete-type
                     (unless (gethash type cast-types)
                       (error "couldn't cast ~s to ~s" (debug-type-names type)
                              (debug-type-names cast-types)))))))
           (c (in-type constraint) out-casts)
           (c (out-type constraint) in-casts)
           (handle-fixed-constraint)))))))

(defmethod update-constraint ((constraint same-type-or-scalar-constraint))
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (flatten-types constraint)
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle same-type-or-scalar-constraint from any-type?"))
  (labels ((scalar (type)
             (aref (scalar/vector-set type) 1))
           (handle-fixed-constraint ()
             ;; can't unify types since they might not have same
             ;; base type. once ctype is down to a single type we can
             ;; remove the constraint since any types remaining in
             ;; OTHER-TYPE should satisfy the constraint, and
             ;; something else should complain if it ends up empty
             (cond
               ((= 1 (hash-table-count (types (ctype constraint))))
                (when (typep (other-type constraint) 'constrained-type)
                  (remhash constraint (constraints (other-type constraint))))
                (setf (ctype constraint)
                      (unify (ctype constraint)
                             (get-only-hash-key (types (ctype constraint)))))
                t)
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve =s constraint?")))))
    (cond
      ;; expand any-type when other-type is constrained
      ((typep (ctype constraint) 'any-type)
       (change-class (ctype constraint) 'constrained-type)
       (maphash (lambda (k v)
                  (when v
                    ;; add type and scalar version
                    (setf (gethash (scalar k) (types (ctype constraint)))
                          t)
                    (setf (gethash k (types (ctype constraint)))
                          t)))
                (types (other-type constraint))))
      (t
       ;; otherwise, loop through both types, removing any that don't match
       (let ((valid-types (make-hash-table))
             (ctype-types (types (ctype constraint)))
             (other-type-types (types (other-type constraint)))
             (ctype-removed 0)
             (other-type-removed 0))
         (maphash (lambda (k v)
                    (if (and v
                             (or (gethash k ctype-types)
                                 (gethash (scalar k) ctype-types)))
                        (setf (gethash k valid-types) t
                              (gethash (scalar k) valid-types) t)
                        (progn
                          (remhash k other-type-types)
                          (incf other-type-removed))))
                  other-type-types)
         (maphash (lambda (k v)
                    (unless (and v
                                 (gethash k valid-types))
                      (remhash k ctype-types)
                      (incf ctype-removed)))
                  ctype-types)
         (when (plusp ctype-removed)
           (flag-modified-type (ctype constraint)))
         (when (plusp other-type-removed)
           (flag-modified-type (other-type constraint))))))
    ;; collapse constraint if possible
    (handle-fixed-constraint)))

;; fixme: see if this can be simplified, and/or factor out common
;; parts with other constraints?
(defmethod update-constraint ((constraint scalar-type-of-constraint))
  (flatten-types constraint)
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle scalar-type-of-constraint from any-type?"))
  (labels ((scalar (type)
             (aref (scalar/vector-set type) 1))
           (handle-fixed-constraint ()
             ;; can't unify types since they might not have same
             ;; base type. once ctype is down to a single type we can
             ;; remove the constraint since any types remaining in
             ;; OTHER-TYPE should satisfy the constraint, and
             ;; something else should complain if it ends up empty
             (cond
               ((= 1 (hash-table-count (types (ctype constraint))))
                (when (typep (other-type constraint) 'constrained-type)
                  (remhash constraint (constraints (other-type constraint))))
                (setf (ctype constraint)
                      (unify (ctype constraint)
                             (get-only-hash-key (types (ctype constraint)))))
                t)
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve s constraint?")))))
    (cond
      ((typep (other-type constraint) 'concrete-type)
       ;; expand any-type when other-type is concrete
       (error "not done yet..."))
      ;; expand any-type when other-type is constrained
      ((typep (ctype constraint) 'any-type)
       (change-class (ctype constraint) 'constrained-type)
       (maphash (lambda (k v)
                  (when v
                    ;; add scalar version
                    (setf (gethash (scalar k) (types (ctype constraint)))
                          t)))
                (types (other-type constraint))))
      (t
       ;; otherwise, loop through both types, removing any that don't match
       (let ((valid-types (make-hash-table))
             (ctype-types (types (ctype constraint)))
             (other-type-types (types (other-type constraint)))
             (ctype-removed 0)
             (other-type-removed 0))
         (maphash (lambda (k v)
                    (if (and v (gethash (scalar k) ctype-types))
                        (setf (gethash (scalar k) valid-types) t)
                        (progn
                          (remhash k other-type-types)
                          (incf other-type-removed))))
                  other-type-types)
         (maphash (lambda (k v)
                    (unless (and v (gethash k valid-types))
                      (remhash k ctype-types)
                      (incf ctype-removed)))
                  ctype-types)
         (when (plusp ctype-removed)
           (flag-modified-type (ctype constraint)))
         (when (plusp other-type-removed)
           (flag-modified-type (other-type constraint))))))
    ;; collapse constraint if possible
    (handle-fixed-constraint)))


(defmethod update-constraint ((constraint same-size-different-base-type-constraint))
  (flatten-types constraint)
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle same-size-different-base-type-constraint from any-type?"))
  (labels ((handle-fixed-constraint ()
             ;; can't unify types since they don't generally have same
             ;; base type. once ctype is down to a single type we can
             ;; remove the constraint since any types remaining in
             ;; OTHER-TYPE should satisfy the constraint, and
             ;; something else should complain if it ends up empty
             (cond
               ((= 1 (hash-table-count (types (ctype constraint))))
                (when (typep (other-type constraint) 'constrained-type)
                  (remhash constraint (constraints (other-type constraint))))
                (setf (ctype constraint)
                      (unify (ctype constraint)
                             (get-only-hash-key (types (ctype constraint)))))
                t)
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve =# constraint?")))))
    (cond
      ((typep (other-type constraint) 'concrete-type)
       ;; expand any-type when other-type is concrete
       (error "not done yet...")
       )
      ;; expand any-type when other-type is constrained
      ((typep (ctype constraint) 'any-type)
       (change-class (ctype constraint) 'constrained-type)
       (maphash (lambda (k v)
                  (when v
                    (setf (gethash
                           (aref (scalar/vector-set (base-type
                                                     constraint))
                                 (scalar/vector-size k))
                           (types (ctype constraint)))
                          t)))
                (types (other-type constraint))))
      (t
       ;; otherwise, loop through both types, removing any that don't match
       (let ((valid-counts (make-array 5 :initial-element nil)) ;; indices 1-5
             (used-counts (make-array 5 :initial-element nil))
             (ctype-types (types (ctype constraint)))
             (other-type-types (types (other-type constraint)))
             (ctype-removed 0)
             (other-type-removed 0))
         (maphash (lambda (k v)
                    (when v (setf (aref valid-counts (scalar/vector-size k)) t)))
                  ctype-types)
         (maphash (lambda (k v)
                    (if (and v (aref valid-counts (scalar/vector-size k)))
                        (setf (aref used-counts (scalar/vector-size k)) t)
                        (progn
                          (remhash k other-type-types)
                          (incf other-type-removed))))
                  other-type-types)
         (maphash (lambda (k v)
                    (unless (and v (aref used-counts (scalar/vector-size k)))
                      (remhash k ctype-types)
                      (incf ctype-removed)))
                  ctype-types)
         (when (plusp ctype-removed)
           (flag-modified-type (ctype constraint)))
         (when (plusp other-type-removed)
           (flag-modified-type (other-type constraint))))))
    ;; if other-type is concrete or we updated type down to 1,
    ;; collapse constraint
    (handle-fixed-constraint)))

(defmethod update-constraint ((constraint same-base-type-different-size-constraint))
  (flatten-types constraint)
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle same-base-type-different-size-constraint from any-type?"))
  (labels ((handle-fixed-constraint ()
             ;; can't unify types since they don't generally have same
             ;; type. once ctype is down to a single base type we can
             ;; remove the constraint since any types remaining in
             ;; OTHER-TYPE should satisfy the constraint, and
             ;; something else should complain if it ends up empty
             (cond
               ((= 1 (hash-table-count (types (ctype constraint))))
                (when (typep (other-type constraint) 'constrained-type)
                  (remhash constraint (constraints (other-type constraint))))
                (setf (ctype constraint)
                      (unify (ctype constraint)
                             (get-only-hash-key (types (ctype constraint)))))
                t)
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve .xyz constraint?")))))
    (cond
      ((typep (other-type constraint) 'concrete-type)
       (let ((removed 0))
         (maphash (lambda (k v)
                    (unless (and v
                                 (eq k
                                     (aref (scalar/vector-set
                                            (other-type constraint))
                                           (out-size constraint))))
                      (incf removed)
                      (remhash k (types (ctype constraint)))))
                  (types (ctype constraint)))
         (when (plusp removed)
           (flag-modified-type (ctype constraint)))))
      ;; expand any-type when other-type is constrained
      ((typep (ctype constraint) 'any-type)
       (change-class (ctype constraint) 'constrained-type)
       (maphash (lambda (k v)
                  (when v
                    (setf (gethash
                           (aref (scalar/vector-set k) (out-size constraint))
                           (types (ctype constraint)))
                          t)))
                (types (other-type constraint))))
      (t
       ;; otherwise, loop through both types, removing any that don't match
       (let ((used-types (make-hash-table))
             (ctype-types (types (ctype constraint)))
             (other-type-types (types (other-type constraint)))
             (ctype-removed 0)
             (other-type-removed 0))
         ;; remove invalid types from other-types, flag valid types for ctype
         (maphash (lambda (k v)
                    (let ((k/size (aref (scalar/vector-set k)
                                        (out-size constraint))))
                      (if (and v
                               (>= (scalar/vector-size k)
                                   ;; can't use swizzle on scalar, so min size 2
                                   (max 2 (min-size constraint)))
                               (gethash k/size ctype-types))
                          (setf (gethash k/size used-types) t)
                          (progn
                            (remhash k other-type-types)
                            (incf other-type-removed)))))
                  other-type-types)
         ;; remove invalid types from ctypes
         (maphash (lambda (k v)
                    (unless (and v (gethash k used-types))
                      (remhash k ctype-types)
                      (incf ctype-removed)))
                  ctype-types)
         (when (plusp ctype-removed)
           (flag-modified-type (ctype constraint)))
         (when (plusp other-type-removed)
           (flag-modified-type (other-type constraint))))))
    ;; if we only have 1 type left for ctype, collapse constraint
    (handle-fixed-constraint)))


(defun run-type-inference ()
  (when *verbose*
    (format t "~&running type inference, ~s elements in worklist~%"
           (length *inference-worklist*)))
  (loop with start-count = (length *inference-worklist*)
        for constraint = (pop *inference-worklist*)
        while constraint
        for i from 1
        when (zerop (mod i 2000))
          do (break "~a loops in inference?" i)
        do (assert (modified constraint))
        when *verbose*
          do (format t "~&~%~s: updating constraint ~s (~s left)~%"
                     i constraint (length *inference-worklist*))
           (dump-constraint constraint)
        do (update-constraint constraint)
           ;; clear flag after updating so it doesn't get put back
           ;; when constrained types change
           (setf (modified constraint) nil)
        when *verbose*
          do (format t "~&==updated constraint:~%")
             (dump-constraint constraint)
        finally (progn
                  (when *verbose*
                    (format t "~&finished type inference, ~s updates~%" i))
                  (return (list start-count i)))))

(defun infer (function)
  (let* ((*inference-worklist* nil)
         (*current-function-local-types* (local-binding-type-data function))
         (ret (walk function (make-instance 'infer-build-constraints))))
    ;; leaves are at end of list, so we process them before interior nodes
    ;; (should be correct either way, but wastes effort doing interior first)
    (setf *inference-worklist* (nreverse *inference-worklist*))
    (run-type-inference)
    (when (or (not (value-type function))
              (eq (value-type function) t))
      (setf (value-type function)
            (get-type-binding :void)))
    ret))


(defun infer-modified-functions (functions)
  ;; find dependents of modified functions, and add to a list in
  ;; dependency order
  (when *verbose*
    (format t "infer-modified-functions ~s~%" functions))
  (let ((in (make-hash-table))
        (out (make-hash-table))
        (leaves nil))
    ;; build local copy of dependency graph including modified
    ;; functions and all dependents
    (labels ((walk (f &optional up)
               (unless (gethash f in)
                 (setf (gethash f in) (make-hash-table)
                       (gethash f out) (make-hash-table))
                 (maphash (lambda (k v)
                            (declare (ignore v))
                            (setf (gethash k (gethash f out)) t)
                            (walk k f))
                          (function-dependents f)))
               (when up
                 (setf (gethash up (gethash f in)) t))))
      (mapcar #'walk functions))
    ;; topo sort function list
    (loop for c = 0
          while (plusp (hash-table-count out))
          do (maphash (lambda (k v)
                        ;; find leaves
                        (when (zerop (hash-table-count v))
                          (incf c)
                          (push k leaves)
                          ;; remove them from table
                          (remhash k out)
                          ;; and from dependents of their dependencies
                          (maphash (lambda (k2 v2)
                                     (declare (ignore v2))
                                     (remhash k (gethash k2 out)))
                                   (gethash k in))))
                      out)
          when (zerop c) do (error "?~s ~s ~s" in out leaves))
    ;; run type inference on list
    (loop for function in leaves
          ;; when (function-type-changed function)
          ;; assuming all stages have compatible uniforms (has to be
          ;;   true for anything linked into a single program anyway),
          ;;   so we can just pick a stage
          collect (handler-case
                      (let ((*current-shader-stage* (car (valid-stages function))))
                        (infer function)
                        (setf (type-inference-state function) t))
                    (inference-failure ()
                      (setf (type-inference-state function) :failed)))
          do
             ;; not sure if we should check type-inference-state here?
             ;; assuming it THROWs to higher level for now...

             ;; mark dependents as needing inference
             (loop for k being the hash-keys of (function-dependents
                                                 function)
                   do (setf (type-inference-state k) nil)))
    ;; return list of modified functions
    leaves))
#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (declare (:float b))
                    (glsl::vec3 a b)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (declare (:float b))
                    (+ (glsl::vec3 a b) 2.0)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (declare (:float b))
                    (+ (glsl::mat3 a b) 2)))
                'h
                :vertex))


#++
(multiple-value-list
 (compile-block '((defun h (a)
                    (declare (:int a))
                    (< a 2)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (declare (:float b))
                    (= (+ a 2) b)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (declare (:vec2 b))
                    (equal a b)))
                   'h
                   :vertex))


#++
(multiple-value-list
 (compile-block '((defun h (a b c)
                    (declare (:vec2 c) (:float a b))
                    (glsl::smooth-step a b c)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a b c)
                    (declare (:vec2 c) (:float a))
                    (+ a b c)))
                   'h
                   :vertex))


#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (let* ((c (+ a 2))
                           (d (+ c b)))
                      (declare (:int c) (:vec2 d))
                      (+ a b c d))))
                'h
                :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (declare (:ivec3 a))
                    (length a)))
                'h
                :vertex))



#++
(multiple-value-list
 (compile-block '((defun h (a b)
                    (if a (+ b 2) (- b 3))))
                'h
                :vertex))

#++
(multiple-value-list
 (compile-block '((defun h ()
                    (let ((a))
                      (setf a (1+ (glsl::vec4 1 2 3 4))))))
                'h
                :vertex))

#++
(multiple-value-list
 (compile-block '((defun h ()
                    (let ((a))
                      (setf (.rb a) (1+ (glsl::vec2 1 2))))))
                'h
                :vertex))

#++
(print
 (multiple-value-list
  (compile-block '((defun h ()
                     (declare (values))
                     (let ((a))
                       (setf (.b a) 1
                             (.r a) 2)
                       (if (>= (.b a) 1)
                           (return (glsl:vec4 1 2 3 4))
                           (return a)))))
                 'h
                 :vertex)))


#++
(print
 (multiple-value-list
  (compile-block '((defun h ()
                     (return (glsl:vec2 1))
                     (return (glsl:ivec2 1))))
                 'h
                 :vertex)))


#++
(print
 (multiple-value-list
  (compile-block '((defun p1 (g)
                     (return (- (* 1.2 g) 1))
                     (return 1))
                   (defun h ()
                     (let ((a (p1 1.0))))))
                 'h
                 :vertex)))

#++
(print
 (multiple-value-list
  (compile-block '((defun p1 (g)
                     (if g
                         (return (glsl::ivec2 1 2))
                         (return (glsl::vec2 1 2))))
                   (defun h ()
                     (let ((a (p1 (> 1 2))))
                       ;(return a)
                       )))
                 'h
                 :vertex)))
#++
(print
 (multiple-value-list
  (compile-block '((defun p1 (a b)
                     (let ((tmp))
                       (setf tmp (- 1 (* a b)))
                       (return tmp)))
                   (defun h ()
                     (let ((a (p1 1.0 2.0)))
                       )))
                 'h
                 :vertex)))

#++
(print
 (multiple-value-list
  (compile-block '((defun h ()
                     (let ((a 1.0)
                           (cl-user::a 2))
                       (let ((a 2)
                             (b a))
                         (setf cl-user::a (+ a b))
                         (let ((b 1)
                               (a 2))
                           (setf cl-user::a (+ cl-user::a a b))))
                       (return (+ a cl-user::a)))))
                 'h
                 :vertex)))


#++
(print
 (multiple-value-list
  (compile-block '((defun a (g)
                     (declare (values :dvec2))
                     (let ((a))
                       (declare (:float a))
                      (if g
                          (return (glsl::ivec2 1 2))
                          (return (glsl::vec2 1 2)))))
                   (defun h ()
                     (let ((a (a (> 1 2))))
                       (return (values))
                       )))
                 'h
                 :vertex)))
