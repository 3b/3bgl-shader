(in-package #:3bgl-shaders)

(defvar *inference-worklist*)
(defvar *current-function-constraint*)
(defvar *current-function-stages*)
(defclass any-type ()
  ((constraints :initform (make-hash-table) :accessor constraints :initarg :constraints)))


(define-condition inference-failure () ())
(define-condition incomplete-dependent (inference-failure) ())

(defclass optional-arg-type ()
  ;; used to indicate optional args, replaced by actual type
  ;; when actual number of args passed is known
  ((arg-type :initarg :arg-type :accessor arg-type)))

;;; would be better to have a singleton for the 'nil type', since we
;;; want to look for it in hash tables, so rather than having an
;;; actual singleton instance of 'nil-type', just using actual NIL for now...
;;; (have to watch out for things that will interpret it as a list though)
#++
(defclass nil-type () ;; for optional args
  ())

(defclass constrained-type ()
  ;; constraints shouldn't include equality constraints, they get
  ;; handled in UNIFY
  ((types :accessor types :initarg :types
          :initform (make-hash-table))
   (constraints :initform (make-hash-table) :accessor constraints :initarg :constraints)))


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
    (optional-arg-type
     (format nil "(or NIL ~s)" (debug-type-names (arg-type type))))
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
  (format t "added constraint to type ~s?~%" ctype))
(defmethod add-constraint ((ctype constrained-type) constraint)
  (setf (gethash constraint (constraints ctype)) t))
(defmethod add-constraint ((ctype any-type) constraint)
  (setf (gethash constraint (constraints ctype)) t))
(defmethod add-constraint ((ctype optional-arg-type) constraint)
  (add-constraint (arg-type ctype) constraint))


;; having a separate constant-type instead of a set-type with 1 type seems
;; to complicate things more than it helps...
#++
(defclass constant-type (constrained-type)
  ((type :initarg :type :accessor ctype)))

(defclass constraint ()
  ((modified :initform nil :accessor modified)))

(defun flag-modified-constraint (constraint)
  (unless (modified constraint)
    (format t "~&flag constraint ~s~%" constraint)
    (setf (modified constraint) t)
    (push constraint *inference-worklist*)))

(defun flag-modified-type (type)
  (format t "~&flag type ~s (~s)~%" type (debug-type-names type))
  (maphash (lambda (constraint v)
             (when v (flag-modified-constraint constraint)))
           (constraints type)))

(defclass function-application (constraint)
  ((argument-types :accessor argument-types :initform nil)
   (return-type :initarg :return-type :accessor return-type) ;; SET-TYPE?
   (function-types :initarg :function-types :accessor function-types) ;; modifiable list of ftype, each is a list of concrete arg types + concrete ret type
   (name :initarg :name :accessor name :initform nil) ;; for debugging
   ))

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
   (out-type :initarg :out :accessor out-type)))

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


(defmethod replace-constraint-type (new old constraint)
  ;; shouldn't modify CONSTRAINTS field of callers, since they may
  ;; be iterating it... wait for update pass before removing any
  ;; no-longer-useful constraints
  (format t "replace ~s with ~s in constraint ~s~%" old new constraint)
  (when (and (slot-boundp constraint 'return-type)
             (eq (return-type constraint) old))
    (flag-modified-constraint constraint)
    (setf (return-type constraint) new))
  (when (position old (argument-types constraint))
    (flag-modified-constraint constraint)
    (setf (argument-types constraint)
          (substitute new old (argument-types constraint)))))

(defmethod replace-constraint-type (new old (constraint ctype/other-constraint))
  (when (eq (ctype constraint) old)
    (flag-modified-constraint constraint)
    (setf (ctype constraint) new))
  (when (eq (other-type constraint) old)
    (flag-modified-constraint constraint)
    (setf (other-type constraint) new))
)

(defmethod replace-constraint-type (new old (constraint cast-constraint))
  (when (eq (in-type constraint) old)
    (flag-modified-constraint constraint)
    (setf (in-type constraint) new))
  (when (eq (out-type constraint) old)
    (flag-modified-constraint constraint)
    (setf (out-type constraint) new)))


(defparameter *copy-constraints-hash* nil
  "used to track already copied constraints when copying type inference data")

(defun update-constraint-cache (new old)
  (when (and *copy-constraints-hash*
             (gethash old *copy-constraints-hash*))
    (setf (gethash old *copy-constraints-hash*) new)))

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
    ;; replace B with A
    (format t "replacing/aa ~s with ~s~%" b a)
    (loop for c being the hash-keys of (constraints b) using (hash-value v)
          do (format t "  ~s? in ~s~%" v c)
          when v
            do (replace-constraint-type a b c)
             (unless (gethash c (constraints a))
               (setf (gethash c (constraints a)) t))
               (setf (gethash c (constraints a)) t)))
  (update-constraint-cache a b)
  a)

(defmethod unify ((a constrained-type) (b constrained-type))
  ;; merge sets of types
  ;; if singleton
  ;;    replace with concrete-type
  ;;    remove A,B from constraint, add new type
  ;;    flag constraints modified
  ;;    return new type
  ;; else
  ;;    update A with new set of types
  ;;    set constraints of A to union of A, B constraints
  ;;    remove A,B from constraints, add A
  ;;      (or remove B, add A if not already there)
  ;;    flag constraint modified
  ;;    return A

  (let ((types (make-hash-table))
        (last-type nil))
    (maphash (lambda (k v) (when (and v (gethash k (types a)))
                             (setf last-type k)
                             (setf (gethash k types) t)))
             (types b))
    (case (hash-table-count types)
      (0 (error "failed to unify types ~s, ~s" a b))
      (1
       (loop for c being the hash-keys of (constraints a) using (hash-value v)
             when v
               do (replace-constraint-type last-type a c))
       (loop for c being the hash-keys of (constraints b) using (hash-value v)
             when v
               do (replace-constraint-type last-type b c))
       )
      (t
       (setf (types a) types)
       (loop for c being the hash-keys of (constraints b) using (hash-value v)
             when v
               do (unless (gethash c (constraints a))
                    (setf (gethash c (constraints a)) t)
                    (replace-constraint-type a b c))))))
  (update-constraint-cache a b)
  a)

(defmethod2 unify ((a constrained-type) (b generic-type))
  ;; make sure B is in A's set of valid types
  ;; remove A,B from constraints, add B
  ;; flag constraint modified
  ;; return B
  (error "constrained x generic not done yet")
)

(defmethod2 unify ((a constrained-type) (b concrete-type))
  (assert (gethash b (types a)))
  (format t "replacing1 ~s with ~s~%" a b)
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type b a c))
  (update-constraint-cache b a)
  b)
(defmethod2 unify ((a any-type) (b concrete-type))
  (assert (gethash b (types a)))
  (format t "replacing2 ~s with ~s~%" a b)
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type b a c)
             (unless (gethash c (constraints b))
               (setf (gethash c (constraints b)) t)))
  (update-constraint-cache b a)
  b)


(defmethod2 unify ((a constrained-type) (b any-type))
  ;; replace B with A in all of its constraints (if not already there)
  ;; and add Bs constraints to A
  (format t "replacing3 ~s with ~s~%" b a)
  (loop for c being the hash-keys of (constraints b) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type a b c)
             (setf (gethash c (constraints a)) t))
  (update-constraint-cache a b)
  a)



(defmethod2 unify ((a constrained-type) (b null))
  (assert (gethash b (types a)))
  (format t "replacing4 ~s with ~s~%" a b)
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type b a c))
  (update-constraint-cache b a)
  b)



(defclass infer-build-constraints (glsl::glsl-walker)
  ())

(defun set-type (types &key constraint)
  (typecase types
    ((eql t)
     (make-instance 'any-type
                    :constraints (alexandria:plist-hash-table
                                  (when constraint (list constraint t)))))
    (symbol
     #++(make-instance 'constant-type :type types)
     (set-type (list (or (get-type-binding types) types))
               :constraint constraint))
    (concrete-type
     ;; fixme: combine the inference types with other type stuff properly
     ;;(set-type (list (name types)))
     types)
    (list
     (let ((set (make-hash-table)))
       (loop for i in types
             when (symbolp i)
               do (setf i (or (get-type-binding i) i))
             do (setf (gethash i set) t))
       (make-instance 'constrained-type :types set
                      :constraints (alexandria:plist-hash-table
                                    (when constraint (list constraint t))))))))

(defmethod copy-constraints :around (x)
  ;; we might have NIL as cached value, so check 2nd value of gethash
  (multiple-value-bind (cached found)
      (gethash (if (typep x 'generic-type)
                   (get-equiv-type x)
                   x)
               *copy-constraints-hash*)
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
    (flag-modified-constraint copy)
    copy)
  )

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
    (flag-modified-constraint copy)
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
    (flag-modified-constraint copy)
    copy)
  )

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
    (flag-modified-constraint copy)
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
    (flag-modified-constraint copy)
    copy))

(defmethod copy-constraints ((constraint cast-constraint))
  (let ((copy (make-instance 'cast-constraint
                             :cast-type (cast-type constraint))))
    ;; we need to update cache before copying constrained types because they
    ;; link back to constraint
    (setf (gethash constraint *copy-constraints-hash*)
          copy)
    (format t "copy cast constraint ~s -> ~s~%"
            (debug-type-names (in-type constraint))
            (debug-type-names (out-type constraint)))
    (setf (slot-value copy 'in-type)
          (copy-constraints (in-type constraint)))
    (setf (slot-value copy 'out-type)
          (copy-constraints (out-type constraint)))
    (flag-modified-constraint copy)
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
    (flag-modified-constraint copy)
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
    (flag-modified-constraint copy)
    copy))

(defmethod copy-constraints ((type generic-type))
  ;; concrete types and aggregates don't need copied
  (let ((e (or (get-equiv-type type) type)))
    (setf (gethash type *copy-constraints-hash*) e
          (gethash e *copy-constraints-hash*) e)))

(defmethod copy-constraints ((type null))
  ;; doesn't need copied
  (setf (gethash type *copy-constraints-hash*) type))

#++
(defmethod copy-constraints ((type set-type))
  (error "shouldn't get a bare set-type in copy-constraints?"))

(defmethod copy-constraints ((hash hash-table))
  (let ((n (make-hash-table)))
    (maphash (lambda (k v)
               (when v
                 (setf (gethash (copy-constraints k) n) t)))
             hash)
    n))

(defmethod copy-constraints ((type constrained-type))
  (let* ((copy (make-instance 'constrained-type)))
    ;; we need to update cache before copying constraints because they
    ;; link back to type
    (setf (gethash type *copy-constraints-hash*) copy)
    ;; assuming we don't have any equiv types in constraint graphs being
    ;; copied (and too messy to fix it here, so just complaining for now)
    ;;(assert (eq type (get-equiv-type type)))
    ;; (setf (equiv copy) copy)
    (setf (slot-value copy 'constraints)
          (copy-constraints (constraints type)))
    (setf (slot-value copy 'types)
          (copy-constraints (types type)))
    copy))

(defun expand-optional-arg-type (o-a-t)
  (if (typep o-a-t 'optional-arg-type)
      (let ((new (arg-type o-a-t)))
        (format t "expand optional type ~s to ~s~%" o-a-t new)
        (when (or (typep new 'any-type)
                  (typep new 'constrained-type))
          (maphash (lambda (c v) (when v
                                   (replace-constraint-type new o-a-t c)))
                   (constraints new)))
        new)
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


(defun copy-unify-constraints (type unify-type &key cast)
  (format t "~&c-u-c ~s ~s~%" type unify-type)
  (unless unify-type
    (assert (typep type 'optional-arg-type))
    (return-from copy-unify-constraints nil))
  (let ((copy (copy-constraints type)))
    (format t "~&c-unify ~s ~s~%" copy unify-type)
    (if cast
        (let ((cast-constraint (make-instance 'cast-constraint
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
  (call-next-method)
)

(defun cast-to-boolean (type)
  ;; no implicit casts to bool, so just force type to be bool...
  (let ((bool (get-type-binding :bool)))
    (etypecase type
      (any-type
       (change-class type 'constrained-type)
       (setf (gethash bool (types type)) t)
       (flag-modified-type type))
      (constrained-type
       (assert (gethash bool (types type)))
       (clrhash (types type))
       (setf (gethash bool (types type)) t)
       (flag-modified-type type))
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
         (valid-vector-types (loop for i from (max 2 (1+ (min-size form))) below 5
                                   append (aref vector-types i))))
    (setf (value-type form)
          (cond
            ;; check for known output type
            ((typep binding-type 'concrete-type)
             (aref (scalar/vector-set binding-type) n))
            ((and (typep binding-type 'constrained-type)
                  (= 1 (hash-table-count (types binding-type))))
             (maphash (lambda (k v)
                        (when v
                          (return-from walk
                            (setf (value-type form)
                                  (aref (scalar/vector-set k) n)))))
                      (types binding-type)))
            (t
             (let ((c (make-instance 'same-base-type-different-size-constraint
                                     :out-size n
                                     :min-size (1+ (min-size form))
                                     :ctype out-type
                                     :other-type binding-type))
                   (gfc (make-instance 'global-function-constraint
                                       :name 'swizzle
                                       :function form)))
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
               (add-constraint binding-type gfc)
               (add-constraint out-type gfc)
               (setf (argument-types gfc) (list binding-type)
                     (return-type gfc) out-type)
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
    (break "slot-access" (list form struct-type) )
)
)
(defmethod walk ((form function-call) (walker infer-build-constraints))
  (format t "~&infer ~s (~s) =~%" (name (called-function form)) form)
  (let* ((called (called-function form))
         (*copy-constraints-hash* (make-hash-table)))
    (when (typep called 'unknown-function-binding)
      (error "got call to unknown function ~s during type inference"
             (name called)))
    (unless (eq t (type-inference-state called))
      (format t "got call to function ~s with incomplete or failed type inference ~s?" (name called) (type-inference-state called))
      (error 'incomplete-dependent))
    ;; make local copies of any types/constraints affected by function
    ;; args, and unify with actual arg types
    ;; (unify will add modified constraints to work list)
    (format t "ibc / walk function ~s,~% args=~s,~% bindings=~s~%"
            (name called)
            (arguments form)
            (bindings called))
    (assert (<= (length (arguments form))
                (length (bindings called))))
    ;; store NIL in the cache for any unused args, so we don't try to copy them
    ;; while walking other args
    (loop for binding in (nthcdr (length (arguments form)) (bindings called))
          do (setf (gethash (value-type binding) *copy-constraints-hash*) nil))

    ;; walk remaining args and copy as usual
    (loop with args = (arguments form)
          for arg = (pop args)
          for binding in (bindings called)
          collect (copy-unify-constraints
                   (value-type binding)
                   (if arg
                       (walk arg walker)
                       nil)
                   :cast (and arg (allow-casts binding))))
    ;; copy return type and any linked constraints
    ;; (may have already been copied if it depends on arguments)
    (format t "~&~s =>~s~%" (name called) (value-type called ))
    (if (eq (called-function form)
            (get-function-binding 'return
                                  :env glsl::*glsl-base-environment*))
        ;; if we are calling RETURN, unify return type with function
        ;; return as well
        (let* ((r (copy-constraints (value-type called))))
          (if (slot-boundp *current-function-constraint* 'return-type)
              (setf (return-type *current-function-constraint*)
                    (unify r (return-type
                              *current-function-constraint*)))
              (setf (return-type *current-function-constraint*) r))
          (add-constraint (return-type *current-function-constraint*)
                          *current-function-constraint*)
          ;; fixme: decide what to do with type returned to caller, since
          ;; it doesn't actually get returned?
          (return-type *current-function-constraint*))
        ;; normal function, just copy the return values as usual
        (copy-constraints (value-type called)))))

(defmethod walk ((form variable-read) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (walk (binding form) walker))

(defmethod walk ((form variable-write) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (let* ((binding (walk (binding form) walker))
         (value (walk (value form) walker))
         ;; todo: avoid creating cast constraint if we know both types?
         (cast (make-instance 'cast-constraint
                              :in value
                              :out binding)))
    (assert value)
    (add-constraint binding cast)
    (add-constraint value cast)
    (flag-modified-constraint cast)
    value))

(defmethod walk ((form local-variable) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (value-type form)))

(defmethod walk ((form binding) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (value-type form)))


(defmethod walk ((form interface-binding) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print
   (let ((stage-binding (stage-binding form)))
     (assert stage-binding)
     (walk stage-binding walker))))

(defmethod walk ((form interface-stage-binding) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (walk (binding form) walker)))

(defmethod walk ((form constant-binding) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (if (member (value-type form) '(t nil))
      (walk (initial-value-form form) walker)
      (value-type form)))

(defmethod walk ((form function-argument) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (value-type form))
)

(defmethod walk ((form integer) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (set-type (list :int))))

(defmethod walk ((form float) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print  (set-type :float)))

(defmethod walk ((form explicit-progn) (walker infer-build-constraints))
  (format t "@infer ~s~%" form)
  (loop for a in (body form)
        for ret = (walk a walker)
        finally (return ret)))

(defmethod walk ((form binding-scope) (walker infer-build-constraints))
  (format t "infer ~s  (~s)=~%" form (body form))
;  (break "foo " form)
  (let ((c (make-instance 'global-function-constraint
                          :name 'let
                          :function form)))
    (setf (argument-types c)
          (loop for binding in (bindings form)
                for declared-type = (if (eq t (declared-type binding))
                                        (make-instance 'any-type)
                                        (set-type (list (declared-type binding))))
                for initial-value-type = (walk (initial-value-form binding) walker)
                do (format t "binding ~s -> ~s(~s) @ ~s~%"
                           (name binding) (debug-type-names initial-value-type)
                           initial-value-type
                           (initial-value-form binding))
                when initial-value-type
                  do (let ((cast (make-instance 'cast-constraint
                                                :cast-type :implicit
                                                :in initial-value-type
                                                :out declared-type)))
                       (add-constraint declared-type cast)
                       (add-constraint initial-value-type cast)
                       (flag-modified-constraint cast)
                       (flag-modified-constraint c))
                do (add-constraint declared-type c)
                collect (setf (value-type binding) declared-type)))
    (setf (return-type c)
          (loop for a in (body form)
                for ret = (walk a walker)
                finally (return ret)))))

(defmethod walk ((form global-function) (walker infer-build-constraints))
  (format t "infer ~s  (~s)=~%" form (body form))
  (let* ((c (make-instance 'global-function-constraint
                          :name (name form)
                          :function form))
         (*current-function-constraint* c)
         (*current-function-stages* (list t)))
    (setf (argument-types c)
          (loop for binding in (bindings form)
                for declared-type = (declared-type binding)
                ;; fixme: is this still right/useful?
                if (typep declared-type '(cons (eql T) (cons unsigned-byte)))
                  collect (setf (value-type binding)
                                (value-type (elt (bindings form)
                                                 (second declared-type))))
                else collect (setf (value-type binding)
                                   (set-type declared-type
                                             :constraint c))))
    ;; fixme: add a constraint (or just a type?) if return type isn't T
    (assert (or (eq t (value-type form))
                (typep (value-type form) 'any-type)))
    (loop for a in (body form)
          do (walk a walker))
    (let ((v (if (and (value-type form)
                      (not (eq (value-type form) t)))
                 (value-type form)
                 (make-instance 'any-type))))
      (format t "set return type to ~s~%" v)
      (print
       (if (slot-boundp c 'return-type)
           (setf (value-type form) (unify (return-type c) v))
           (setf (return-type c) v))))
    c))


(defmethod walk (form (walker infer-build-constraints))
  (format t "unhandled form ~s~%" form)
  (when (next-method-p)
    (call-next-method))
)

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
  (or (eq a b) (eq (get-equiv-type a) b)))

(defmethod2 unifiable-types-p ((a generic-type) (b generic-type))
  (eq (or (get-equiv-type a) a) (or (get-equiv-type b) b)))

(defmethod2 unifiable-types-p ((a generic-type) (b constrained-type))
  (gethash (get-equiv-type a) (types b)))

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


(defmethod update-constraint ((constraint function-application))
  ;; fixme: keep track of which args were modified and only process them...
  ;; fixme: rearrange data structures to avoid copies here?
  (let ((removed 0)
        (removed2 0)
        (arg-type-counts (make-array (length (argument-types constraint))
                                     :initial-element 0)))
    ;; no point in processing a T*->T function, since it won't
    ;; restrict any of the arguments or return type, so it shouldn't
    ;; have a constraint in the first place...
    (assert (not (eql t (function-types constraint))))
    (loop for a in (argument-types constraint)
          for i from 0
          when (typep a 'constrained-type)
            do (setf (aref arg-type-counts i) (hash-table-count (types a))))
    (format t "~&update constraint ~s (~s ftypes)~%" constraint (length (function-types constraint)))
    (format t "  constrained = ~{~s~%                 ~}-> ~s~%"
            (argument-types constraint)
            (return-type constraint))
    (print-bindings/ret (name constraint) (argument-types constraint) (return-type constraint))
    ;; loop through function types, keep ones that match the argument/ret types
    (setf (function-types constraint)
          (loop for ftype in (function-types constraint)
                for (args ret) = ftype
                do (format t "  check constraint ~s: " ftype)
                when (prin1
                      (and
                       ;; fixme: store concrete types in ftypes instead of symbols
                       (unifiable-types-p ret (return-type constraint))
                       #++(gethash (or (get-type-binding ret) ret)
                                (types (return-type constraint)))

                       (loop for .ftype-arg = (pop args)
                             for ftype-arg = (or (get-type-binding .ftype-arg)
                                                 .ftype-arg)
                             for arg in (argument-types constraint)
                             always (unifiable-types-p arg ftype-arg)
                                    #++(or (typep arg 'any-type)
                                           ;; possibly should add a TYPES
                                           ;; reader to constant-types to
                                           ;; simplify this?
                                           (if (typep arg 'concrete-type)
                                               (eql ftype-arg arg)
                                               (gethash ftype-arg (types arg)))))))
                  collect ftype
                else do (incf removed)
                do (terpri)))
    (format t " -> (~s ftypes)~%" (length (function-types constraint)))
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
    (flet ((hash-list (l)
             (let ((h (make-hash-table)))
               (loop for i in l do (setf (gethash i h) nil))
               h)))
      (when (typep (return-type constraint) 'any-type)
        (break "any type?")
        (change-class (return-type constraint)
                      'set-type
                      :types (hash-list (mapcar 'second
                                                (function-types constraint)))))
      (loop for arg in (argument-types constraint)
            for i from 0
            do (format t "maybe expand any type ~s?~%" arg)
            when (typep arg 'any-type)
              do (change-class arg 'blahblah
                               :types (hash-list
                                       (mapcar (lambda (a) (nth i (first a)))
                                               (function-types constraint))))

            ))

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
          ;do (format t "add ftype? ~s -> ~s~%" args ret)
          if (typep (return-type constraint) 'concrete-type)
            do (assert (unifiable-types-p ret (return-type constraint)))
          else
            do (assert (nth-value 1 (gethash ret (types (return-type constraint)))))
          do (loop with arg-types = (argument-types constraint)
                   for .ftype-arg in args
                   for ftype-arg = (or (get-type-binding .ftype-arg)
                                       .ftype-arg)
                   for arg = (pop arg-types)
                   ;;do (format t "add arg type? ~s -> ~s~%" ftype-arg arg)
                   unless (typep arg 'concrete-type)
                     do (assert (nth-value 1 (gethash ftype-arg (types arg))))
                        (setf (gethash ftype-arg (types arg)) t)
                        (incf removed2)))
    ;; flag any modified types
    (loop for a in (argument-types constraint)
          for i from 0
          when (and (typep a 'constrained-type)
                    (/= (aref arg-type-counts i)
                        (hash-table-count (types a))))
            do (flag-modified-type a))

    ;; (possibly loop through arg types again and remove NIL values?
    (format t "updated constraint, removed ~s ftypes, ~s arg types~%" removed removed2)
    (print-bindings/ret (name constraint) (argument-types constraint) (return-type constraint))
    #++(assert (or (plusp removed) (plusp removed2)))
    (unless (or (plusp removed) (plusp removed2))
      (warn "couldn't narrow constraint?"))
    )

)

(defmethod update-constraint ((constraint cast-constraint))
  (let ((in-casts (make-hash-table))
        (out-casts (make-hash-table))
        (any-in (typep (in-type constraint) 'any-type))
        (any-out (typep (out-type constraint) 'any-type)))
    (format t "update cast constraint ~s:~%  ~s -> ~s~%"
            constraint
            (debug-type-names(in-type constraint))
            (debug-type-names(out-type constraint)))
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
               (when (or (and (print (not any-in))
                              (zerop (hash-table-count in-casts)))
                         (and (print (not any-out))
                              (zerop (hash-table-count out-casts))))
                 (error "can't resolve constraint?"))
               (when (and (or any-in (= 1 (hash-table-count in-casts)))
                          (or any-out (= 1 (hash-table-count out-casts))))
                 ;; constraint is an equality constraint, just unify the types
                 (unless (typep (in-type constraint) 'concrete-type)
                   (remhash constraint (constraints (in-type constraint))))
                 (unless (typep (out-type constraint) 'concrete-type)
                   (remhash constraint (constraints (out-type constraint))))
                 (unify (in-type constraint) (out-type constraint))
                 (format t "constraint resolved to ~s~%"
                         (debug-type-names (in-type constraint)))
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
             (not (print "  update any-type/out in cast"))
             (unify (out-type constraint)
                    (make-instance 'constrained-type
                                   :types in-casts))))
        ((or (not (in-type constraint))
             (not (out-type constraint)))
         (unify (in-type constraint)
                (out-type constraint)))
        (t
         (flet ((c (type cast-types)
                  (when (typep type 'constrained-type)
                    (let ((old (hash-table-count (types type))))
                      (format t "try cast cast ~s -> ~s~%"
                              (debug-type-names type)
                              (mapcar 'name
                                      (alexandria:hash-table-keys cast-types)))
                      (maphash (lambda (k v)
                                 (unless (and v (gethash k cast-types))
                                   (remhash k (types type))))
                               (types type))
                      (assert (plusp (hash-table-count (types type))))
                      (when (/= old (hash-table-count (types type)))
                        (flag-modified-type type))))))
           (c (in-type constraint) out-casts)
           (c (out-type constraint) in-casts)
           (handle-fixed-constraint))))

      (format t "  updated cast constraint:~%  ~s -> ~s~%"
              (debug-type-names(in-type constraint))
              (debug-type-names(out-type constraint)))
)))

(defmethod update-constraint ((constraint same-type-or-scalar-constraint))
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle same-type-or-scalar-constraint from any-type?"))
  (format t "=s constraint: ~s -> ~s~%"
          (debug-type-names (other-type constraint))
          (debug-type-names (ctype constraint)))
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
                (when (or (typep (ctype constraint) 'any-type)
                          (typep (ctype constraint) 'constrained-type))
                  (remhash constraint (constraints (ctype constraint)))))
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve =s constraint?")))
             (format t "  updated =s constraint: ~s -> ~s~%"
                     (debug-type-names (other-type constraint))
                     (debug-type-names (ctype constraint)))))
    (cond
      ((typep (other-type constraint) 'concrete-type)
       ;; expand any-type when other-type is concrete
       (error "not done yet..."))
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
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle scalar-type-of-constraint from any-type?"))
  (format t "s constraint: ~s -> ~s~%"
          (debug-type-names (other-type constraint))
          (debug-type-names (ctype constraint)))
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
                (when (or (typep (ctype constraint) 'any-type)
                          (typep (ctype constraint) 'constrained-type))
                  (remhash constraint (constraints (ctype constraint)))))
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve s constraint?")))
             (format t "  updated s constraint: ~s -> ~s~%"
                     (debug-type-names (other-type constraint))
                     (debug-type-names (ctype constraint)))))
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
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle same-size-different-base-type-constraint from any-type?"))
  (format t "=# constraint: ~s/~s -> ~s~%"
          (debug-type-names (other-type constraint))
          (debug-type-names (base-type constraint))
          (debug-type-names (ctype constraint)))
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
                (when (or (typep (ctype constraint) 'any-type)
                          (typep (ctype constraint) 'constrained-type))
                  (remhash constraint (constraints (ctype constraint)))))
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve =# constraint?")))
             (format t "  updated =# constraint: ~s/~s -> ~s~%"
                     (debug-type-names (other-type constraint))
                     (debug-type-names (base-type constraint))
                     (debug-type-names (ctype constraint)))))
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
  ;; not allowing any-type for other-type for now, since constraint
  ;; only makes sense for some types, so it should have been
  ;; restricted to those types in definition
  (when (typep (other-type constraint) 'any-type)
    (error "don't know how to handle same-base-type-different-size-constraint from any-type?"))
  (format t ".xyz constraint: ~s/~s -> ~s/~s~%"
          (debug-type-names (other-type constraint))
          (min-size constraint)
          (out-size constraint)
          (debug-type-names (ctype constraint)))
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
                (when (typep (ctype constraint) 'constrained-type)
                  (remhash constraint (constraints (ctype constraint)))))
               ((or (zerop (hash-table-count (types (ctype constraint))))
                    (zerop (hash-table-count (types (other-type constraint)))))
                (error "can't resolve .xyz constraint?")))
             (format t "  updated .xyz constraint: ~s/~s -> ~s/~s~%"
                     (debug-type-names (other-type constraint))
                     (min-size constraint)
                     (out-size constraint)
                     (debug-type-names (ctype constraint)))))
    (cond
      ((typep (other-type constraint) 'concrete-type)
       (let ((removed 0))
         (maphash (lambda (k v)
                    (unless (and v
                                 (eq k
                                     (aref (scalar/vector-set
                                            (other-type constraint))
                                           (out-size constraint))))
                      (format t "check type ~s / ~s (~s)~%"
                              (name k)
                              (name (aref (scalar/vector-set
                                           (other-type constraint))
                                     (out-size constraint)))
                              v)
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

(defmethod update-constraint ((constraint global-function-constraint))
  ;;(break "foo" constraint)
  (unless (typep (global-function constraint) 'place)
    (loop for arg in (argument-types constraint)
          for binding in (bindings (global-function constraint))
          do (setf (value-type binding) arg)))
  (when (or (typep (global-function constraint) 'global-function)
            (typep (global-function constraint) 'place))
    (setf (value-type (global-function constraint))
          (return-type constraint)))
  (format t "updated global-function-constraint ~s:~%" (name constraint))
  (print-bindings/ret (name constraint) (argument-types constraint) (return-type constraint)))


(defun infer (function)
  (let* ((*inference-worklist* nil)
         (gfc (walk function (make-instance 'infer-build-constraints))))
    ;; leaves are at end of list, so we process them before interior nodes
    ;; (should be correct either way, but wastes effort doing interior first)
    (setf *inference-worklist* (nreverse *inference-worklist*))
    (loop for constraint = (pop *inference-worklist*)
          while constraint
          for i from 1
          when (zerop (mod i 1000))
            do (break "~a loops in inference?" i)
          do (assert (modified constraint))
          do (update-constraint constraint)
             ;; clear flag after updating so it doesn't get put back
             ;; when constrained types change
             (setf (modified constraint) nil))
    (loop for b in (bindings function)
          for type in (argument-types gfc)
          do (setf (value-type b) type))
    ;; store return type of function
    (setf (value-type function) (return-type gfc))
    (when (or (not (value-type function))
              (eq (value-type function) t))
      (setf (value-type function)
            (get-type-binding :void)))
    (print-bindings/ret (name function) (bindings function)
                        (value-type function))
    (print-bindings/ret (name function) (argument-types gfc)
                        (return-type gfc))
    (return-type gfc)))


(defun infer-modified-functions (functions)
  ;; find dependents of modified functions, and add to a list in
  ;; dependency order
  (format t "infer-modified-functions ~s~%" functions)
  (let ((in (make-hash-table))
        (out (make-hash-table))
        (leaves nil))
    ;; build local copy of dependency graph including modified
    ;; functions and all dependents
    (labels ((walk (f &optional up)

               (format t "walk ~s~@[ @ ~s~] ~%" (name f) (and up (name up)))
               (unless (gethash f in)
                 (setf (gethash f in) (make-hash-table)
                       (gethash f out) (make-hash-table))
                 (maphash (lambda (k v)
                            (declare (ignore v))
                            (setf (gethash k (gethash f out)) t)
                            (walk k f))
                          (function-dependents f)))
               (when up
                 (format t "add ~s <- ~s~%" (name f) (name up))
                 (setf (gethash up (gethash f in)) t))))
      (mapcar #'walk functions))
    (loop for (k . v) in (alexandria:hash-table-alist in)
          do (format t "~s <-~{ ~s~}~%" (name k)
                     (mapcar 'name (alexandria:hash-table-keys v))))

    (loop for (k . v) in (alexandria:hash-table-alist out)
          do (format t "~s ->~{ ~s~}~%" (name k)
                     (mapcar 'name (alexandria:hash-table-keys v))))
    ;; topo sort function list
    (loop for c = 0
          while (plusp (hash-table-count out))
          do (maphash (lambda (k v)
                        ;; find leaves
                        (when (zerop (hash-table-count v))
                          (incf c)
                          (format t "~&leaf ~s~%" (name k))
                          (push k leaves)
                          ;; remove them from table
                          (remhash k out)
                          ;; and from dependents of their dependencies
                          (maphash (lambda (k2 v2)
                                     (declare (ignore v2))
                                     (format t "  remove ~s from ~s~%"
                                             (name k) (name k2))
                                     (remhash k (gethash k2 out)))
                                   (gethash k in))
))
                      out)
          when (zerop c) do (error "?~s ~s ~s" in out leaves))
    (print (mapcar 'name leaves))
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
                      (setf (type-inference-state function) :failed))
)
          do
             ;; not sure if we should check type-inference-state here?
             ;; assuming it THROWs to higher level for now...
             (progn ;when (function-type-changed function)
               ;; mark dependents as needing inference
               (loop for k being the hash-keys of (function-dependents
                                                   function)
                     do (setf (type-inference-state k) nil))
               #++(setf (old-function-type function)
                     (function-type function))))
    ;;
    ))
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
