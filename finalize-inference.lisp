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
     (assert (eq type force-type)))
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

(defmethod get-concrete-type ((type concrete-type))
  type)
(defmethod get-concrete-type ((type constrained-type))
  (let ((ct))
    (maphash (lambda (k v) (when v (assert (not ct)) (setf ct k)))
             (types type))
    ct))

(defun flatten-function (function argument-types)
  (setf argument-types (mapcar 'get-concrete-type argument-types))
  (format t "~%~%~%flattening function ~s: ~s~%  ~s~%~%~%~%" (name function)
          (debug-type-names argument-types)
          argument-types)
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

      (when (eq (name function) 'p1)
        (break "finalize1" function local-types))


      ;; assign types to function arguments, rtun type inference if
      ;; any changed
      (assert (= (length argument-types) (length (bindings function))))
      (when (plusp (loop for arg in argument-types
                         for binding in (bindings function)
                         count (flatten-type (gethash binding local-types)
                                             arg)))
        ;; updated arguments
        (print (run-type-inference)))

      (when (eq (name function) 'p1)
        (break "finalize2" function local-types))

      ;; loop through variable bindings
      ;;  if multiple types, collapse to simplest type then run
      ;;  type inference

      ;; loop over keys instead of maphash because inference update
      ;; might modify other parts of hash table
      (loop for k in (alexandria:hash-table-keys local-types)
            for v = (gethash k local-types)
            do (if (typep k '(or local-variable))
                   (progn
                     (format t "update local variable ~s (~s)~%" (name k)
                             (debug-type-names v))
                     (when (flatten-type v)
                       (print (run-type-inference))))
                   (format t "skipping ~s~%" k)))

      ;; loop through function calls
      ;;   pick a type for args, if not cached, recurse
      ;;   update return type, run type inference if changed
      #++
      (loop for k in (alexandria:hash-table-keys local-types)
            for v = (gethash k local-types)
            do (if (typep k 'inference-call-site)
                   (progn
                     (format t "updating ~s~%" (name (called-function k)))
                     (map 'nil #'flatten-type (cdr v))
                     (let ((r (flatten-function (called-function k) (cdr v))))
                       (when (flatten-type (car v) r)
                         (print (run-type-inference)))))
                   (format t "skipping ~s~%" k)))

      ;; flatten return type of function
      (flatten-type (gethash :return local-types))
      (when (typep (gethash :return local-types) 'any-type)
        (setf (gethash :return local-types)
              (get-type-binding :void)))
      ;; build mapping of variables/functions to types for this
      ;; combination of arguments
      (let ((cache (make-hash-table)))
        (maphash (lambda (k v)
                   (if (typep k 'inference-call-site)
                       (progn
                         (format t "use function ~s: ~s~%"
                                 (name (called-function k))
                                 (debug-type-names v))
                         (pushnew (mapcar (lambda (a)
                                            (flatten-type a)
                                            (get-concrete-type a))
                                          (cdr v))
                                  (gethash k cache nil) :test 'equal)
                         (format t "-> ~s~%"
                                 (debug-type-names (gethash k cache :?)))
                         #++(break "flat?" function k v local-types))
                       (setf (gethash k cache) v)))
                 local-types)
        (setf (gethash argument-types (final-binding-type-cache function))
              cache))))
  (format t "~%~% flattened ~s: ~s -> ~s~%~s~%"
          (name function) (debug-type-names argument-types)
          (debug-type-names
           (gethash :return (gethash argument-types
                                     (final-binding-type-cache function))))
          argument-types)
  #++(break "flat?" function)
  ;; add any used function signatures to the hash table for printing
  (maphash (lambda (k v)
             (when (typep k 'inference-call-site)
               (flatten-function (called-function k) (car v))
               #++(unless (gethash (called-function k) *instantiated-overloads*)
                 (setf (gethash (called-function k) *instantiated-overloads*)
                       (make-hash-table :test #'equal)))
               ;; for each called function, we keep track of which
               ;; sets of types were used (possibly there are usually
               ;; few enough combinations that pushnew would be better
               ;; than making a hash table here and converting to a
               ;; list later?
               #++(setf (gethash v (gethash (called-function k)
                                         *instantiated-overloads*))
                     t)))
           (gethash argument-types (final-binding-type-cache function)))
;  (break "flat?" function)
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
    (format t "~%~%~%~%~%~%~%")

    (flatten-function root nil)

    ;; reformat the data for printer to use
    #++
    (maphash (lambda (k v)
               (setf (gethash k *instantiated-overloads*)
                     (loop for (r b) in (alexandria:hash-table-values v)
                           do (setf (gethash k b) r)
                           collect b)))
             *instantiated-overloads*)
    (maphash (lambda (k v)
               (setf (gethash k *instantiated-overloads*)
                     (alexandria:hash-table-keys v)))
             *instantiated-overloads*)
    *instantiated-overloads*))
