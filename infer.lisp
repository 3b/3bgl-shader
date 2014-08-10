(in-package #:3bgl-shaders)

(defvar *inference-worklist*)

(defclass any-type ()
  ((constraints :initform (make-hash-table) :accessor constraints :initarg :constraints)))

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
    (t (error "foo!"))))

(defun print-bindings/ret (name bindings ret)
  (when name (format t "inferred ~s:~%" name))
  (loop for i in bindings
        do (format t "  ~a = ~s~%" (if (typep i 'generic-type)
                                       (name i)
                                       i)
                   (debug-type-names (if (typep i 'binding)
                                         (value-type i)
                                         i))))
    (format t "  -> ~s~%" (debug-type-names ret)))


(defun add-constraint (ctype constraint)
  (setf (gethash constraint (constraints ctype)) t))


;; having a separate constant-type instead of a set-type with 1 type seems
;; to complicate things more than it helps...
#++
(defclass constant-type (constrained-type)
  ((type :initarg :type :accessor ctype)))

(defclass constraint ()
  ((modified :initform t :accessor modified)))

(defun flag-modified-constraint (constraint)
  (unless (modified constraint)
    (setf (modified constraint) t)
    (push constraint *inference-worklist*)))

(defclass function-application (constraint)
  ((argument-types :accessor argument-types :initform nil)
   (return-type :initarg :return-type :accessor return-type) ;; SET-TYPE?
   (function-types :initarg :function-types :accessor function-types) ;; modifiable list of ftype, each is a list of concrete arg types + concrete ret type
   (name :initarg :name :accessor name :initform nil) ;; for debugging
   ))

(defclass global-function-constraint (constraint)
  ;; doesn't actually constrain things, just a place to track changes
  ;; to argument bindings' types
  ((name :accessor name :initarg :name :initform nil)
   (argument-types :accessor argument-types :initform nil)
   (return-type :initarg :return-type :accessor return-type)))

(defun replace-constraint-type (new old constraint)
  (when (eq (return-type constraint) old)
    (setf (return-type constraint) new))
  (flag-modified-constraint constraint)
  (setf (argument-types constraint)
        (substitute new old (argument-types constraint))))

(defclass cast-constraint (constraint)
  ;; represents a possible implicit cast from IN-TYPE to OUT-TYPE
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
  ((in-type :initarg :in-type :accessor in-type)
   (out-type :initarg :out-type :accessor out-type)))



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
                    (break "?")
                    (remhash b (types c))
                    (replace-constraint-type a b c)))
       ))
)
  a


)

(defmethod2 unify ((a constrained-type) (b generic-type))
  ;; make sure B is in A's set of valid types
  ;; remove A,B from constraints, add B
  ;; flag constraint modified
  ;; return B
  (error "constrained x generic not done yet")
)

(defmethod2 unify ((a constrained-type) (b concrete-type))
  (assert (gethash b (types a)))
  (format t "replacing ~s with ~s~%" a b)
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type b a c))
  b)
(defmethod2 unify ((a any-type) (b concrete-type))
  (assert (gethash b (types a)))
  (format t "replacing ~s with ~s~%" a b)
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type b a c))
  b)


(defmethod2 unify ((a constrained-type) (b any-type))
  ;; replace B with A in all of its constraints (if not already there)
  ;; and add Bs constraints to A
  (format t "replacing ~s with ~s~%" b a)
  (loop for c being the hash-keys of (constraints b) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type a b c)
             (setf (gethash c (constraints a)) t))
  a)



(defmethod2 unify ((a constrained-type) (b null))
  (assert (gethash b (types a)))
  (format t "replacing ~s with ~s~%" a b)
  (loop for c being the hash-keys of (constraints a) using (hash-value v)
        do (format t "  ~s? in ~s~%" v c)
        when v
          do (replace-constraint-type b a c))
  b)



(defclass infer-build-constraints (glsl::glsl-walker)
  ())

#++
(defun copy-type (type)
  (if (consp type)
      (copy-list type)
      type))


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
             do (setf (gethash i set) i))
       (make-instance 'constrained-type :types set
                      :constraints (alexandria:plist-hash-table
                                    (when constraint (list constraint t))))))))

(defparameter *copy-constraints-hash* nil
  "used to track already copied constraints when copying type inference data")

(defmethod copy-constraints :around (x)
  (or (gethash (if (typep x 'generic-type)
                   (get-equiv-type x)
                   x)
               *copy-constraints-hash*)
      (call-next-method)))

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
    (push copy *inference-worklist*)
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


(defun copy-unify-constraints (type unify-type)
  (let ((copy (copy-constraints type)))
    (format t "c-unify ~s ~s~%" copy unify-type)
    (unify copy unify-type)))

(defmethod walk ((form function-call) (walker infer-build-constraints))
  (format t "infer ~s (~s) =~%" (name (called-function form)) form)
  (let* ((called (called-function form))
         (*copy-constraints-hash* (make-hash-table)))
    (when (typep called 'unknown-function-binding)
      (error "got call to unknown function ~s during type inference"
             (name called)))
    (unless (eq t (type-inference-state called))
      (format t "got call to function ~s with incomplete or failed type inference ~s?" (name called) (type-inference-state called))
      (throw :incomplete-dependent called))
    ;; make local copies of any types/constraints affected by function
    ;; args, and unify with actual arg types
    ;; (unify will add modified constraints to work list)
    (format t "ibc / walk function ~s,~% args=~s,~% bindings=~s~%"
            (name called)
            (arguments form)
            (bindings called))
    (assert (<= (length (arguments form))
                (length (bindings called))))
    (loop with args = (arguments form)
          for arg = (pop args)
          for binding in (bindings called)
          collect (copy-unify-constraints
                   (value-type binding)
                   (if arg
                       (walk arg walker)
                       nil)))
    ;; copy return type and any linked constraints
    ;; (may have already been copied if it depends on arguments)
    (format t "~s =>~s~%" (name called) (value-type called ))
    (copy-constraints (value-type called))))

(defmethod walk ((form variable-read) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (value-type (binding form)))
)

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

(defmethod walk ((form global-function) (walker infer-build-constraints))
  (format t "infer ~s  (~s)=~%" form (body form))
  (let ((c (make-instance 'global-function-constraint)))
    (setf (return-type c) (set-type t :constraint c))
    (setf (argument-types c)
          (loop for binding in (bindings form)
                for declared-type = (declared-type binding)
                if (typep declared-type '(cons (eql T) (cons unsigned-byte)))
                  collect (setf (value-type binding)
                                (value-type (elt (bindings form)
                                                 (second declared-type))))
                else collect (setf (value-type binding)
                                   (set-type declared-type
                                             :constraint c))))
    ;; fixme: add a constraint (or just a type?) if return type isn't T
    (assert (eq t (value-type form)))
    (setf (return-type c)
          (loop for a in (body form)
                for ret = (walk a walker)
                do (format t "@ global-function / ~s~%" ret)
                finally (return (setf (value-type form) ret))))
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


(defmethod update-constraint ((constraint function-application))
  ;; fixme: keep track of which args were modified and only process them...
  ;; fixme: rearrange data structures to avoid copies here?
  (let ((removed 0)
        (removed2 0))
    ;; no point in processing a T*->T function, since it won't
    ;; restrict any of the arguments or return type, so it shouldn't
    ;; have a constraint in the first place...
    (assert (not (eql t (function-types constraint))))
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
            do (setf (gethash ret
                              (types (return-type constraint)))
                     ret))
    (loop for ftype in (function-types constraint)
          for (args .ret) = ftype
          for ret = (or (get-type-binding .ret) .ret)
          do (format t "add ftype? ~s -> ~s~%" args ret)
          if (typep (return-type constraint) 'concrete-type)
            do (assert (unifiable-types-p ret (return-type constraint)))
          else
            do (assert (nth-value 1 (gethash ret (types (return-type constraint)))))
            do (loop with arg-types = (argument-types constraint)
                     for .ftype-arg in args
                     for ftype-arg = (or (get-type-binding .ftype-arg)
                                         .ftype-arg)
                     for arg = (pop arg-types)
                     do (format t "add arg type? ~s -> ~s~%" ftype-arg arg)
                     unless (typep arg 'concrete-type)
                       do (assert (nth-value 1 (gethash ftype-arg (types arg))))
                          (setf (gethash ftype-arg (types arg))
                                ftype-arg)
                          (incf removed2)))
    ;; (possibly loop through arg types again and remove NIL values?
    (format t "updated constraint, removed ~s ftypes, ~s arg types~%" removed removed2)
    (print-bindings/ret (name constraint) (argument-types constraint) (return-type constraint))
    (assert (or (plusp removed) (plusp removed2)))
    )

)

(defmethod update-constraint ((constraint global-function-constraint))
  (format t "updated global-function-constraint ~s:~%" (name constraint))
  (print-bindings/ret (name constraint) (argument-types constraint) (return-type constraint)))

(defun infer (function)
  #++(format t "~&infer ~s (~s ~s)~%" (name function)
          (function-type function)
          (old-function-type function))
  (let* ((*inference-worklist* nil)
         (gfc (walk function (make-instance 'infer-build-constraints))))
    ;; leaves are at end of list, so we process them before interior nodes
    ;; (should be correct either way, but wastes effort doing interior first)
    (setf *inference-worklist* (nreverse *inference-worklist*))
    (loop for constraint = (pop *inference-worklist*)
          while constraint
          do (update-constraint constraint)
             ;; clear flag after updating so it doesn't get put back
             ;; when constrained types change
             (setf (modified constraint) nil))
    (loop for b in (bindings function)
          for type in (argument-types gfc)
          do (setf (value-type b) type))
    (setf (value-type function) (return-type gfc))
    (print-bindings/ret (name function) (bindings function) (return-type gfc))
    (return-type gfc)
)
)
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
            collect (infer function)
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
;                    (declare (:float b))
                    (+ (glsl::vec4 a b) 2)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a)
                    (declare (:int a))
                    (< a 2)))
                   'h
                   :vertex))
