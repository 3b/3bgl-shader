(in-package #:3bgl-shaders)

;;; try to resolve a call graph to static types

(defclass finalize (glsl::glsl-walker)
  ())


;; hash of function object -> equal-hash of (list of concrete types ->
;;  (concrete return type + hash of bindings -> concrete types?))
(defvar *instantiated-overloads*)

;; hash of binding-object -> type, object removed at end of binding scope
(defvar *binding-types*)

(defmethod flatten-cast-type ((type concrete-type))
  type)
(defmethod flatten-cast-type ((type constrained-type))
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
                   (let ((in (mapcar 'car
                                     (remove nil (alexandria:hash-table-alist
                                                  (types (in-type k)))
                                             :key 'cdr))))
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
        for type = (flatten-cast-type (value-type binding))
        do (format t "  ~s(~s) -> ~s @ ~s~%" (name binding) binding
                   (debug-type-names type)
                   *binding-types*)
           (setf (gethash binding *binding-types*)
                    type))
  (call-next-method))

(defmethod walk ((form slot-access) (walker finalize))
  (format t "slot-access ~s~%" form)
  (call-next-method))

(defmethod walk ((form integer) (walker finalize))
  (if (> form (expt 2 31))
      (get-type-binding :uint)
      (get-type-binding :int)))

(defmethod walk ((form variable-read) (walker finalize))
  (format t "variable-read ~s -> ~s~%" form (debug-type-names
                                             (gethash (binding form)
                                                      *binding-types* :???)))
  (gethash (binding form) *binding-types* :???))

(defmethod walk ((form variable-write) (walker finalize))
  (format t "variable-write ~s~%" form)
  (call-next-method))

(defmethod walk ((form binding) (walker finalize))
  (format t "binding ~s~%" form)
  (call-next-method))

(defmethod walk ((form constant-binding) (walker finalize))
  (format t "constant-binding ~s~%" form)
  (call-next-method))


(defmethod walk ((form interface-binding) (walker finalize))
  (format t "interface-binding ~s~%" form)
  (call-next-method))

(defmethod flatten-internal-function ((c function-application) arg-types)
  (format t "fif ~s / ~s -> " (name c) (mapcar 'name arg-types))
  (prin1
   ;; check for a direct match for function signature
   (loop with args = (mapcar 'name arg-types)
         for ftype in (function-types c)
         when (equalp (car ftype) args)
           return (get-type-binding (second ftype)))))


(defmethod flatten-internal-function ((c same-size-different-base-type-constraint) arg-types)
    (break "same-size-different-base-type-constraint" c))
(defmethod flatten-internal-function ((c same-type-or-scalar-constraint) arg-types)
  (break "same-type-or-scalar-constraint" c))
(defmethod flatten-internal-function ((c scalar-type-of-constraint) arg-types)
  (break "scalar-type-of-constraint" c))
(defmethod flatten-internal-function ((c cast-constraint) arg-types)
    (break "cast-constraint" c))


(defmethod flatten-function-call ((function internal-function) arg-types walker)
  (etypecase (value-type function)
    (concrete-type
     (value-type function))
    (constrained-type
     ;; find a function application constraint or matrix constructor constraint
     ;; and try to get a type from that
     (maphash (lambda (k v)
                (when (and v (typep k '(or function-application
                                        same-size-different-base-type-constraint
                                        same-type-or-scalar-constraint
                                        scalar-type-of-constraint
                                        cast-constraint)))
                  (let ((type (flatten-internal-function k arg-types)))
                    (when (typep type 'concrete-type)
                      (return-from flatten-function-call type)))))
              (constraints (value-type function)))
     ;; if we couldn't get a match from simple expansions,
     ;; copy the constraints and unify and see if it gets a result
     (let* ((*copy-constraints-hash* (make-hash-table))
            (*inference-worklist* ())
            (ret (copy-constraints (value-type function))))
       ;; fixme: this is duplicated from function-call/infer-build-constraints
       ;; walk method, refactor it...

       ;; store NIL in the cache for any unused args, so we don't try
       ;; to copy them while walking other args
       (loop for binding in (nthcdr (length arg-types) (bindings function))
             do (setf (gethash (value-type binding) *copy-constraints-hash*)
                      nil)
                (when (typep (value-type binding) 'optional-arg-type)
                  (setf (gethash (arg-type (value-type binding))
                                 *copy-constraints-hash*)
                        nil)))
       ;; walk remaining args and copy as usual
       (loop with args = arg-types
             for arg = (pop args)
             for binding in (bindings function)
             collect (copy-unify-constraints
                      (value-type binding)
                      arg
                      :cast (and arg (allow-casts binding))))
       ;; update the constraints
       (loop for constraint = (pop *inference-worklist*)
             while constraint
             do (update-constraint constraint)
                (setf (modified constraint) nil))
       (or (flatten-cast-type ret)
           (break "couldn't flatten internal function?"))))))

(defmethod flatten-function-call ((function global-function) arg-types walker)
  ;; make sure we have an entry in function hash for this function
  (let ((cache (or (gethash function *instantiated-overloads*)
                   (setf (gethash function *instantiated-overloads*)
                         (make-hash-table :test #'equal)))))
    ;; see if we already processed this type signature for this function
    (when (gethash arg-types cache)
      (format t "call to function (~s ~{~s~^ ~}), reusing ret ~s~%"
              (name function) arg-types (gethash arg-types cache))
      (return-from flatten-function-call (car (gethash arg-types cache))))
    ;; otherwise bind arg types and process it normally
    (let ((*binding-types* (make-hash-table)))
      (loop with %at = arg-types
            for binding in (bindings function)
            for arg-type = (pop %at)
            do (setf (gethash binding *binding-types*) arg-type))
      (let ((ret))
        (loop for f in (body function)
              do (setf ret (walk f walker)))
        (setf (gethash arg-types cache) (list ret *binding-types*))
        (format t "call to function (~s ~{~s~^ ~}), new ret ~s~%"
                (name function) arg-types ret)
        ret))))

(defmethod walk ((form function-call) (walker finalize))
  (format t "function call ~s -> ~s (~s~%" form (name (called-function form))
          (called-function form))

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
