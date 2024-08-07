(in-package #:3bgl-shaders)

;;; try to resolve a call graph to static types

(defclass finalize (3bgl-glsl::glsl-walker)
  ())


;; hash of function object -> equal-hash of (list of concrete types ->
;;  (concrete return type + hash of bindings -> concrete types?))
(defvar *instantiated-overloads*)

;; hash of binding-object -> type, object removed at end of binding scope
(defvar *binding-types*)

(defun cache-binding (binding type)
  (when *verbose*
    (format t "~&setting type for binding ~s to ~s~%"
           (name binding) (name type)))
  (setf (gethash binding *binding-types*) type))

(defmethod flatten-type ((type concrete-type) &optional force-type)
  (etypecase force-type
    (null)
    (concrete-type
     force-type)
    (constrained-type
     (assert (= 1 (hash-table-count (types force-type))))
     force-type)))

(defmethod flatten-type ((type any-type) &optional force-type)
  (etypecase force-type
    (null
     ;; for now assuming an otherwise unconstrained type is 'void'
     ;; since otherwise something should have affected it
     ;; fixme: (not quite correct, since a few functions like = accept
     ;; any-type, so some rare functions could have unconstrained
     ;; types. also need to handle unused arguments)
     (get-type-binding :void))
    (concrete-type
     #++(assert (eq type force-type))
     #++(break "foo" force-type)
     (change-class type 'ref-type :equiv force-type)
     t)
    (constrained-type
     (flatten-type force-type))))

(defmethod flatten-type ((type constrained-type) &optional force-type)
  ;; pick simplest type (fewest components, then least casts to it
  ;; todo: figure out if any other types need rules?
  ;; usually should get single type for samplers or structs
  (let* ((out (mapcar 'car (remove nil (alexandria:hash-table-alist
                                        (types type))
                                   :key 'cdr)))
         (old (length out)))
    (etypecase force-type
      ((or null constrained-type)
       (unless (= 1 old)
         (setf out (sort out (lambda (a b)
                               (if (eql (scalar/vector-size a)
                                        (scalar/vector-size b))
                                   (< (length (implicit-casts-from a))
                                      (length (implicit-casts-from b)))
                                   (if (scalar/vector-size a)
                                       (and (scalar/vector-size b)
                                            (< (scalar/vector-size a)
                                               (scalar/vector-size b)))
                                       t)))))
         (clrhash (types type))
         (when (typep force-type 'constrained-type)
           (setf out (remove-if-not (lambda (a) (gethash a (types force-type)))
                                    out))
           (assert (plusp (length out))))
         (setf (gethash (first out) (types type)) t)
         (flag-modified-type type)
         ;; return t since we modified it
         t))
      (concrete-type
       (assert (gethash force-type (types type)))
       (when (> (hash-table-count (types type)) 1)
         (clrhash (types type))
         (setf (gethash force-type (types type)) t)
         (flag-modified-type type)
         ;; return t since we modified it
         t)))))

(defmethod flatten-type ((type array-type) &optional force-type)
  (declare (ignorable force-type))
  #++(assert (not force-type)) ;; todo...
  (etypecase (base-type type)
    (concrete-type
     nil)
    ;; todo: figure out what other types need handled
    ))

(defmethod flatten-type ((type struct-type) &optional force-type)
  (assert (or (not force-type)
              (eql type (get-equiv-type force-type)))) ;; todo...
  nil)

(defmethod flatten-type ((type ref-type) &optional force-type)
  (flatten-type (equiv type) force-type))

(defmethod get-concrete-type ((type ref-type))
  (get-concrete-type (equiv type)))

(defmethod get-concrete-type ((type concrete-type))
  type)

(defmethod get-concrete-type ((type struct-type))
  type)

(defmethod get-concrete-type ((type array-type))
  type)

(defmethod get-concrete-type ((type any-type))
  t)

(defmethod get-concrete-type ((type constrained-type))
  (let ((ct))
    (maphash (lambda (k v) (when v (assert (not ct)) (setf ct k)))
             (types type))
    ct))

(defun flatten-function (function argument-types)
  (setf argument-types (mapcar 'get-concrete-type argument-types))
  (when *verbose*
    (format t "~%~%~%flattening function ~s: ~s~%  ~s~%~%~%~%" (name function)
            (debug-type-names argument-types)
            argument-types))
  (unless (gethash argument-types (final-binding-type-cache function))
    ;; assume if we have a cached type, all called functions have been
    ;; cached as well.  otherwise, figure out final types for this
    ;; combination of arguments, and process any called functions
    (let* ((local-types (make-hash-table))
           (*current-function-local-types* local-types)
           (*copy-constraints-hash* (make-hash-table))
           (*inference-worklist* nil))
      ;; copy all the types used by the function
      (maphash (lambda (k v)
                 (if (consp v)
                     (setf (gethash k local-types)
                           (mapcar #'copy-constraints v))
                     (setf (gethash k local-types)
                           (copy-constraints v))))
               (local-binding-type-data function))
      ;; fixme: copy-constraints adds things to worklist too agressively
      ;; so go through and undo it...
      ;;(run-type-inference)
      (loop for i = (pop *inference-worklist*)
            while i
            do (setf (modified i) nil))

      ;; assign types to function arguments, run type inference if
      ;; any changed
      (assert (= (length argument-types) (length (bindings function))))
      (when (plusp (loop for arg in argument-types
                         for binding in (bindings function)
                         count (flatten-type (gethash binding local-types)
                                             arg)))
        ;; updated arguments
        (run-type-inference))

      ;; loop through variable bindings
      ;;  if multiple types, collapse to simplest type then run
      ;;  type inference

      ;; loop over keys instead of maphash because inference update
      ;; might modify other parts of hash table
      (loop for k in (alexandria:hash-table-keys local-types)
            for .v = (gethash k local-types)
            for v = (get-equiv-type .v)
            do (unless (eq v .v)
                 (setf (gethash k local-types) v))
               (if (typep k '(or local-variable))
                      (progn
                        (when *verbose*
                          (format t "update local variable ~s (~s)~%" (name k)
                                  (debug-type-names v)))
                        (when (flatten-type v)
                          (run-type-inference)))))

      ;; flatten called function arguments
      ;; (mostly needed for spirv, since it doesn't have implicit casts)
      (loop for k in (alexandria:hash-table-keys local-types)
            for .v = (gethash k local-types)
            for v = (get-equiv-type .v)
            do (unless (eq v .v)
                 (setf (gethash k local-types) v))
               (when (typep k '(or function-call))
                 (when *verbose*
                   (format t "update function-call arguments ~s (~s)~%"
                           (name (called-function k))
                           (debug-type-names v)))
                 (when (plusp (loop for arg in v
                                    when arg
                                    count (flatten-type arg)))
                   (run-type-inference))))

      ;; flatten return type of function
      (flatten-type (gethash :return local-types))
      (when (typep (gethash :return local-types) 'any-type)
        (setf (gethash :return local-types)
              (get-type-binding :void)))
      ;; build mapping of variables/functions to types for this
      ;; combination of arguments
      (let ((cache (make-hash-table)))
        (maphash (lambda (k v)
                   (if (typep k '(or function-call inference-call-site))
                       (progn
                         (when *verbose*
                           (format t "use function ~s: ~s~%"
                                   (name (called-function k))
                                   (debug-type-names v)))
                         (assert (not (gethash k cache nil)))
                         (setf (gethash k cache nil)
                                  (mapcar (lambda (a)
                                            (when a
                                              (flatten-type a)
                                              (get-concrete-type a)))
                                          v))
                         (when *verbose*
                           (format t "-> ~s~%"
                                   (debug-type-names (gethash k cache :?)))))
                       (setf (gethash k cache) v)))
                 local-types)
        (setf (gethash argument-types (final-binding-type-cache function))
              cache))))
  (when *verbose*
    (format t "~%~% flattened ~s: ~s -> ~s~%~s~%"
            (name function) (debug-type-names argument-types)
           (debug-type-names
            (gethash :return (gethash argument-types
                                      (final-binding-type-cache function))))
           argument-types))

  ;; add any used function signatures to the hash table for printing
  (maphash (lambda (k v)
             (when (and (typep k 'inference-call-site)
                        (typep (called-function k) 'global-function))
               (flatten-function (called-function k) (cdr v))))
           (gethash argument-types (final-binding-type-cache function)))
  (unless (gethash function *instantiated-overloads*)
    (setf (gethash function *instantiated-overloads*)
          (make-hash-table :test #'equal)))
  (setf (gethash argument-types (gethash function *instantiated-overloads*))
        t)
  (gethash :return
           (gethash argument-types (final-binding-type-cache function))))


;;; given a 0-arg function, return a hash table of
;;; function names -> list of overloads to instantiate for that function
;;;   hash of binding -> type for that function and all bindings in it

(defun finalize-inference (root)
  (let ((*instantiated-overloads* (make-hash-table))
        (*binding-types* (make-hash-table)))

    (flatten-function root nil)

    ;; reformat the data for printer to use
    (maphash (lambda (k v)
               (setf (gethash k *instantiated-overloads*)
                     (alexandria:hash-table-keys v)))
             *instantiated-overloads*)
    *instantiated-overloads*))
