(in-package #:3bgl-shaders)

;;; try to resolve a call graph to static types

(defclass finalize (glsl::glsl-walker)
  ())


;; hash of function object -> equal-hash of (list of concrete types ->
;;  (concrete return type + hash of bindings -> concrete types?))
(defvar *instantiated-overloads*)

;; hash of binding-object -> type, object removed at end of binding scope
(defvar *binding-types*)

(defun cache-binding (binding type)
  (format t "~&setting type for binding ~s to ~s~%"
          (name binding) (name type))
  (when (eq :mat4x2 (name type))
    (break "mat4x2?"))
  (setf (gethash binding *binding-types*) type))

(defmethod flatten-cast-type ((type concrete-type) in-type)
  type)
(defmethod flatten-cast-type ((type null) (in-type concrete-type))
  in-type)
(defmethod flatten-cast-type ((type constrained-type) in-type)
  ;; shouldn't happen?
  (break "todo"))

(defmethod flatten-cast-type ((out-type constrained-type)
                              (in-type concrete-type))
  ;; find a cast-constraint from in-type to type, pick lowest cast
  ;; from in-type that is in type
  (maphash (lambda (k v)
             (when (and v
                        (typep k 'cast-constraint)
                       ; (eql in-type (in-type k))
                        (eql out-type (out-type k)))
               (case (cast-type k)
                 (:explicit
                  (break "todo"))
                 ((:implicit t)
                  (loop for cast in (cons in-type (implicit-casts-to in-type))
                        when (gethash cast (types out-type))
                          do (return-from flatten-cast-type cast))
                  (error "broken cast?")))))
           (constraints out-type))

  )
(defmethod flatten-cast-type ((type constrained-type) (in-type null))
  ;; if we have a cast constraint with this type as out-type, pick an
  ;; input type
  ;; fixme: optimize the mapcar/remove/alist junk
  (maphash (lambda (k v)
             (when (and v
                        (typep k 'cast-constraint)
                        (eql type (out-type k)))
               (if (and (typep (in-type k) 'concrete-type)
                        (gethash (in-type k) (types type)))
                   (return-from flatten-cast-type
                     (in-type k))
                   (let* ((in-types (if (typep (in-type k) 'concrete-type)
                                        (list (cons (in-type k) t))
                                        (remove nil
                                                (alexandria:hash-table-alist
                                                 (types (in-type k)))
                                                :key 'cdr)))
                          (in (mapcar 'car in-types)))
                     (setf in (sort in #'<
                                    :key (lambda (a)
                                           (length (implicit-casts-from a)))))
                     (loop for i in in
                           when (gethash i (types type))
                             do (return-from flatten-cast-type i))))))
           (constraints type))
  ;; otherwise, just pick lowest type available
  (let ((out (mapcar 'car
                     (remove nil (alexandria:hash-table-alist
                                  (types type))
                             :key 'cdr))))
    (setf out (sort out #'<
                    :key (lambda (a)
                           (length (implicit-casts-from a)))))
    (first out)))

(defmethod walk ((form progn-body) (walker finalize))
  (let ((ret))
    (loop for f in (body form)
          do (setf ret (walk f walker)))
    ret))

(defmethod walk ((form binding-scope) (walker finalize))
  (format t "binding-scope ~s~%" form)
  (loop for binding in (bindings form)
        for in-type = (when (initial-value-form binding)
                        (walk (initial-value-form binding) walker))
        for type = (flatten-cast-type (value-type binding) in-type)
        do (format t "  ~s(~s) -> ~s @ ~s~%" (name binding) binding
                   (debug-type-names type)
                   *binding-types*)
           (cache-binding binding type))
  (call-next-method))

(defmethod walk ((form array-access) (walker finalize))
  (format t "array-access ~s~%" form)
  (walk (binding form) walker))

(defmethod walk ((form slot-access) (walker finalize))
  (format t "slot-access ~s~%" form)
  (let ((struct-type (walk (binding form) walker)))
    ;; fixme: should structs have a hash for O(1) access?
    ;; or should something earlier have already looked up the specific slot?
    (loop for binding in (bindings struct-type)
          do (format t "field = ~s, binding = ~s?~%" (field form)
                     (name binding))
          when (or (eql (name binding) (field form))
                   ;; todo: decide if (.foo struct) accessor should be
                   ;; kept?  if so, should it intern symbols instead
                   ;; of doing string compare?
                   (equal (string (name binding)) (field form)))
            do (return-from walk (value-type binding)))
   (break "finalize/slot-access" (list form struct-type) )
))

(defun flatten-constraints (return-type nil-bindings bindings arg-types)
  (let* ((*copy-constraints-hash* (make-hash-table))
         (*inference-worklist* ())
         (ret (copy-constraints return-type)))
    ;; fixme: this is duplicated from function-call/infer-build-constraints
    ;; walk method, refactor it...

    ;; store NIL in the cache for any unused args, so we don't try
    ;; to copy them while walking other args
    (loop for binding in nil-bindings
          do (setf (gethash (value-type binding) *copy-constraints-hash*)
                   nil)
             (when (typep (value-type binding) 'optional-arg-type)
               (setf (gethash (arg-type (value-type binding))
                              *copy-constraints-hash*)
                     nil)))
    ;; walk remaining args and copy as usual
    (loop with args = arg-types
          for arg = (pop args)
          for binding in bindings
          collect (copy-unify-constraints
                   (value-type binding)
                   arg
                   :cast (and arg (allow-casts binding))))
    ;; update the constraints
    (loop for constraint = (pop *inference-worklist*)
          while constraint
          do (update-constraint constraint)
             (setf (modified constraint) nil))
    (or (flatten-cast-type ret nil)
        (break "couldn't flatten constraint?"))))

(defmethod walk ((form swizzle-access) (walker finalize))
  (let ((binding-type (walk (binding form) walker)))
    (flatten-constraints (value-type form)
                         nil
                         (unless (typep (binding form) 'function-call)
                           (list (binding (binding form))))
                         (list binding-type))
    #++(loop for binding in (bindings struct-type)
          do (format t "field = ~s, binding = ~s?~%" (field form)
                     (name binding))
          when (or (eql (name binding) (field form))
                   ;; todo: decide if (.foo struct) accessor should be
                   ;; kept?  if so, should it intern symbols instead
                   ;; of doing string compare?
                   (equal (string (name binding)) (field form)))
            do (return-from walk (value-type binding)))

))

(defmethod walk ((form integer) (walker finalize))
  (if (> form (expt 2 31))
      (get-type-binding :uint)
      (get-type-binding :int)))

(defmethod walk ((form float) (walker finalize))
  (get-type-binding :float))

(defmethod walk ((form double-float) (walker finalize))
  (get-type-binding :double))


(defmethod walk ((form interface-binding) (walker finalize))
  (let ((stage-binding (stage-binding form)))
    (assert stage-binding)
    (walk stage-binding walker)))

(defmethod walk ((form interface-stage-binding) (walker finalize))
  (walk (binding form) walker))


(defmethod walk ((form variable-read) (walker finalize))
  (etypecase (binding form)
    ((or local-variable function-argument)
     (format t "variable-read ~s -> ~s~%" form (debug-type-names
                                                (gethash (binding form)
                                                         *binding-types* :???)))
     (gethash (binding form) *binding-types* :???))
    (interface-binding (walk (binding form) walker))
    (constant-binding (walk (binding form) walker))))

(defmethod walk ((form variable-write) (walker finalize))
  (format t "variable-write ~s~%" form)
  (walk (value form) walker)
  #++(let* ((binding (walk (binding form) walker))
         (value (walk (value form) walker))
         ;; todo: avoid creating cast constraint if we know both types?
         (cast (make-instance 'cast-constraint
                              :out value
                              :in binding)))
    (add-constraint binding cast)
    (add-constraint value cast)
    (flag-modified-constraint cast)
    value))

(defmethod walk ((form binding) (walker finalize))
  (format t "binding ~s~%" form)
  (call-next-method))

(defmethod walk ((form constant-binding) (walker finalize))
  (format t "constant-binding ~s~%" form)
  (if (member (value-type form) '(t nil))
      (walk (initial-value-form form) walker)
      (value-type form)))


(defmethod flatten-internal-function ((c function-application) arg-types)
  (format t "fif ~s / ~s -> " (name c) (mapcar 'name arg-types))
  (prin1
   ;; check for a direct match for function signature
   (loop with args = (mapcar 'name arg-types)
         with ftypes = (if (typep c 'variable-arity-function-application)
                           (aref (function-types-by-arity c) (length args))
                           (function-types c))
         for ftype in ftypes
         when (equalp (car ftype) args)
           return (get-type-binding (second ftype)))))


(defmethod flatten-internal-function ((c same-size-different-base-type-constraint) arg-types)
    (break "same-size-different-base-type-constraint" c))

(defmethod flatten-internal-function ((c same-type-or-scalar-constraint) arg-types)
  (break "same-type-or-scalar-constraint" c))

(defmethod flatten-internal-function ((c scalar-type-of-constraint) arg-types)
  (cond
    ((typep (other-type c) 'concrete-type)
     (other-type c))
    ((typep (other-type c) 'constrained-type)
     (let ((types))
        (maphash (lambda (k v) (when v (push k types))) (types (other-type c)))
       (when (= 1 (length types))
         (car types))))))
(defmethod flatten-internal-function ((c cast-constraint) arg-types)
    (break "cast-constraint" c))


(defmethod flatten-function-call ((function internal-function) arg-types walker)
  (etypecase (value-type function)
    (concrete-type
     (value-type function))
    ((or constrained-type any-type)
     ;; handle RETURN specially for now...
     (when (eq (name function) 'return)
       (return-from flatten-function-call
         (car arg-types)))
     ;; find a function application constraint or matrix constructor constraint
     ;; and try to get a type from that
     (maphash (lambda (k v)
                (when (and v)
                  (typecase k
                    ((or function-application
                         cast-constraint)
                     (let ((type (flatten-internal-function k arg-types)))
                       (when (typep type 'concrete-type)
                         (return-from flatten-function-call type))))
                    ((or same-size-different-base-type-constraint
                         same-type-or-scalar-constraint
                         scalar-type-of-constraint)
                     (when (eq (value-type function) (ctype k))
                       (let ((type (flatten-internal-function k arg-types)))
                         (when (typep type 'concrete-type)
                           (return-from flatten-function-call type))))))))
              (constraints (value-type function)))
     ;; if we couldn't get a match from simple expansions,
     ;; copy the constraints and unify and see if it gets a result
     (flatten-constraints (value-type function)
                          (nthcdr (length arg-types) (bindings function))
                          (bindings function)
                          arg-types))))

(defmethod flatten-function-call ((function global-function) arg-types walker)
  ;; make sure we have an entry in function hash for this function
  (let ((cache (or (gethash function *instantiated-overloads*)
                   (setf (gethash function *instantiated-overloads*)
                         (make-hash-table :test #'equal)))))
    ;; see if we already processed this type signature for this function
    (when (gethash arg-types cache)
      (format t "call to function (~s ~{~s~^ ~}), reusing ret ~s~%"
              (name function) (mapcar 'name arg-types)
              (gethash arg-types cache))
      (return-from flatten-function-call (car (gethash arg-types cache))))
    ;; otherwise bind arg types and process it normally
    (let ((*binding-types* (make-hash-table)))
      (loop with %at = arg-types
            for binding in (bindings function)
            for arg-type = (pop %at)
            do (cache-binding binding arg-type))
      (let ((ret))
        (loop for f in (body function)
              do (setf ret (walk f walker)))
        (setf (gethash arg-types cache) (list ret *binding-types*))
        (format t "call to function (~s ~{~s~^ ~}), new ret ~s~%"
                (name function) (mapcar 'name arg-types) (name ret))
        ret))))

(defmethod walk ((form function-call) (walker finalize))
  (format t "function call ~s -> ~s (~s~%" form (name (called-function form))
          (called-function form))
  (assert (eq t (type-inference-state (called-function form))))
  (let* ((arg-types (loop for arg in (arguments form)
                          for type = (walk arg walker)
                          do (format t "  ~s -> ~s~%" arg (debug-type-names type))
                          collect type))
         (ret (flatten-function-call (called-function form) arg-types walker))
         )
    (format t " arg types = ~s -> ~s~%" arg-types (debug-type-names ret))
    ;;(break "function call" form)
    ret)
  ;(call-next-method)
  )

;;; given a 0-arg function, return a hash table of
;;; function names -> list of overloads to instantiate for that function
;;;   hash of binding -> type for that function and all bindings in it

(defun finalize-inference (root)
  (let ((*instantiated-overloads* (make-hash-table))
        (*binding-types* (make-hash-table)))
    (format t "~%~%~%~%~%~%~%")

    #++(walk root (make-instance 'finalize))
    (flatten-function-call root nil (make-instance 'finalize))

    ;;(break "finalized " *instantiated-overloads*)
    ;; reformat the data for printer to use
    (maphash (lambda (k v)
               (setf (gethash k *instantiated-overloads*)
                     (loop for (r b) in (alexandria:hash-table-values v)
                           do (setf (gethash k b) r)
                           collect b)))
             *instantiated-overloads*)
    *instantiated-overloads*))
