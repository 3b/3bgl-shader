(in-package #:3bgl-shaders)
;;; add some builtin functions for testing
;;; todo: move these somewhere separate and add missing functions


;; we have 3 general styles of internal function/operators
;; simple case: all args are same type or concrete type
;;   ex: =, fma
;; slightly harder cases: most args same type or concrete
;;   except some are same size from different category
;;     ex: lessThan
;;   or same type or scalar of same type
;;     ex: +, clamp, smoothstep
;; general case: just list all possibly combinations by hand
;;     ex: *, textureSize
(defun add-internal-function/s (name lambda-list arg-types return-type &key glsl-name)
  (let ((fn (make-instance 'internal-function
                           :name name
                           :glsl-name glsl-name
                           :lambda-list lambda-list))
        (types (make-array (count '&optional lambda-list :test-not #'eql)
                           :initial-element nil)))
    ;; 2 passes, so types can be constrained by later args
    ;; (ex smoothstep, first 2 args are constrained by 3rd)
    (setf (bindings fn)
          (loop with optional = nil
                with arg-type = nil
                with i = 0
                for variable-name in lambda-list
                when (eq variable-name '&optional)
                  do (setf optional t)
                else
                  do (setf arg-type (pop arg-types))
                     (typecase arg-type
                       ;; (or ...) list of types
                       ((cons (eql or))
                        (setf (aref types i)
                              ;; no constraints, since we only have
                              ;; equality constraints here, which are
                              ;; just represented with same type
                              ;; object
                              (make-instance
                               'constrained-type
                               :types (alexandria:alist-hash-table
                                       (mapcar (lambda (a)
                                                 (cons (or (get-type-binding a)
                                                           a)
                                                       t))
                                               (cdr arg-type))))))
                       ((eql t)
                        (setf (aref types i)
                              (make-instance 'any-type)))
                       (symbol
                        (setf (aref types i)
                              (or (get-type-binding arg-type)
                                  (error "unknown type ~s?" arg-type)))))
                  and collect (make-instance 'binding
                                             :name variable-name
                                             :value-type (or (aref types i)
                                                             arg-type)
                                             :allow-casts t)
                  and do (incf i)))
    (loop for binding in (bindings fn)
          for arg-type = (value-type binding)
          for i from 0
          when (consp arg-type)
            do (etypecase arg-type
                 ;; (= ##) same types as arg ##
                 ((cons (eql =))
                  (setf (aref types i)
                        (aref types (second arg-type))))
                 ;; (=s ##) same type as or scalar base type of arg ##
                 ((cons (eql =s))
                  (let ((c (make-instance
                            'same-type-or-scalar-constraint
                            :ctype (setf (aref types i)
                                         (make-instance 'any-type))
                            :other-type (aref types (second arg-type)))))
                    (add-constraint (aref types (second arg-type)) c)
                    (add-constraint (aref types i) c)))
                 ;; (=# ## base-type) same element count as arg ## but
                 ;; specified base type (ex :bool => vec3 -> bvec3)
                 ((cons (eql =#))
                  (let ((c (make-instance
                            'same-size-different-base-type-constraint
                            :other-type (aref types (second arg-type))
                            :base-type (get-type-binding (third arg-type))
                            :ctype (setf (aref types i)
                                         (make-instance 'any-type)))))
                    (add-constraint (aref types (second arg-type)) c)
                    (add-constraint (aref types i) c))))
               (setf (value-type binding) (aref types i)))
    (etypecase return-type
      ;; (OR) types not allowed for return type, has to either match
      ;; an arg type or be a specific type
      ((cons (eql =#))
       (let ((c (make-instance 'same-size-different-base-type-constraint
                               :ctype (setf (value-type fn)
                                            (make-instance 'any-type))
                               :other-type (aref types (second return-type))
                               :base-type (get-type-binding
                                           (third return-type)))))
         (add-constraint (aref types (second return-type)) c)
         (add-constraint (value-type fn) c)))
      ((cons (eql =))
       (setf (value-type fn) (aref types (second return-type))))
      (symbol
       (setf (value-type fn) (get-type-binding return-type))))
    (setf (gethash name (function-bindings *environment*))
          fn)))

(defun add-internal-function/mat (name lambda-list count return-type
                                  &key glsl-name)
  ;; matrix constructors types are too complex to enumerate explicitly
  ;; (nearly 3 million for mat3), so we need to use a special
  ;; constraint for them
  ;; (may just accept 1-COUNT of ARG-TYPES for now, eventually should
  ;;  enforce that it is passed either 1 matrix type or a combination
  ;;  of arg-types that adds up to exactly COUNT elements)
  ;; for now just enumerating the combinations of sizes, and handling
  ;;  them like implicit casts except allowing cast to any same-size
  ;;  type still a bit big (24k for mat4, 5.2k at worst arity for
  ;;  mat4), but relatively manageable
  ;; fixme: test performance of stuff with lots of calls to mat4
  (let* ((fn (make-instance 'internal-function
                            :name name
                            :glsl-name glsl-name
                            :lambda-list lambda-list
                            ;; :declared-type ?
                            :value-type (get-type-binding return-type)))
         (arity-types (make-array (1+ count) :initial-element nil))
         (arg-types (make-array (1+ count) :initial-element nil))
         (constraint (make-instance
                      'variable-arity-function-application
                      :name name
                      :return-type (get-type-binding return-type)
                      :function-types-by-arity arity-types))
         (base-types '((:float 1) (:vec2 2) (:vec3 3) (:vec4 4) (:mat2x3 6)
                       (:mat2x4 8) (:mat3 9) (:mat3x4 12) (:mat4 16))))
    ;; accept any of the base types (or anything that casts to them)
    ;; for 1ary function
    (setf (aref arity-types 1)
          (mapcar (lambda (a) (list (list (car a)) return-type))
                  base-types))

    ;; accept any combination of args that adds up to COUNT elements
    ;; for n-ary functions where (<= 2 N COUNT)
    (labels ((vec/mat-constructor (n)
               (if (zerop n)
                   (list nil)
                   (loop for (type count) in base-types
                         when (<= count n)
                           append (mapcar (lambda (a) (cons type a))
                                          (vec/mat-constructor (- n count)))))))
      (loop for type in (vec/mat-constructor count)
            for l = (length type)
            when (> l 1)
              do (push (list type return-type)
                       (aref arity-types l))))

    ;; figure out which types are valid for specific args
    ;; (for example last type is always nil/scalar only)
    ;; fixme: probably should calculate this directly
    (loop for i from 1 to count
          for ftypes = (aref arity-types i)
          do (loop for ftype in ftypes
                   do (loop for j below i
                            do (pushnew (nth j (car ftype))
                                        (aref arg-types j)))))
    #++(break "foo" (list arity-types arg-types))
    ;; update arg types in constraint
    (setf (argument-types constraint)
          (loop for i below count
                for arg-type across arg-types
                for ct = (make-instance
                         'constrained-type
                         :types (alexandria:alist-hash-table
                                 (mapcar (lambda (a)
                                           (cons (or (get-type-binding a)
                                                     a)
                                                 t))
                                         arg-type))
                         :constraints (alexandria:plist-hash-table
                                       (list constraint t)))
                collect (if (plusp i)
                            (make-instance 'optional-arg-type
                                           :arg-type ct)
                            ct)))
    ;; and bindings in fn
    (setf (bindings fn)
            (loop with types = (argument-types constraint)
                  with i = 0
                  for binding in lambda-list
                  unless (eq binding '&optional)
                    collect (make-instance 'binding
                                           :name binding
                                           :value-type (pop types)
                                           :allow-casts :explicit)
                    and do (incf i)))
    ;; and add function binding
   (setf (gethash name (function-bindings *environment*))
         fn)))

(defun add-internal-function/full (name lambda-list type &key glsl-name (allow-casts t))
  (let* ((fn (make-instance 'internal-function
                            :name name
                            :glsl-name glsl-name
                            :lambda-list lambda-list
                            :declared-type type))
         (arity-types (make-array (1+ (length (remove '&optional lambda-list)))
                                  :initial-element nil))
         (constraint (make-instance
                      'variable-arity-function-application
                      :name name
                      :function-types-by-arity arity-types))
         (ret-types (mapcar (lambda (a) (cons (get-type-binding a) t))
                            (delete-duplicates
                             (mapcar 'second type))))
         (ret (if (<= (length ret-types) 1)
                  (caar ret-types)
                  (make-instance 'constrained-type
                                 :types (alexandria:alist-hash-table
                                         ret-types)
                                 :constraints (alexandria:plist-hash-table
                                               (list constraint t))))))

    (loop for ftype in type
          for l = (length (first ftype))
          minimizing l into min
          maximizing l into max
          do (push ftype (aref arity-types l))
          finally (setf (min-arity constraint) min
                        (max-arity constraint) max))

    (when (= (min-arity constraint) (max-arity constraint))
      (change-class constraint 'function-application
                    :function-types type))

    (setf (return-type constraint) ret
          (value-type fn) ret)

    ;; allowing &optional in lambda list, no other l-l-keywords though
    ;; just plain symbols for optional args, no default or -p arg
    (flet ((make-type (n)
             (let* ((types (delete-duplicates
                            (mapcar (lambda (a)
                                      (nth n (car a)))
                                    type)))
                    (type (make-instance 'constrained-type
                                         :types
                                         (alexandria:alist-hash-table
                                          (mapcar (lambda (a)
                                                    (cons (or (get-type-binding a)
                                                              a
                                                              nil)
                                                          t))
                                                  (remove nil types)))
                                         :constraints
                                         (alexandria:plist-hash-table
                                          (list constraint t)))))
               (when (position nil types)
                 (setf type (make-instance 'optional-arg-type
                                           :arg-type type)))
               (push type (argument-types constraint))
               type)))
      (setf (bindings fn)
            (loop with optional = nil
                  with i = 0
                  for binding in lambda-list
                  when (eq binding '&optional)
                    do (setf optional t)
                  else
                    collect (make-instance 'binding
                                           :name binding
                                           :value-type (make-type i)
                                           :allow-casts allow-casts)
                    and do (incf i))))
    (setf (argument-types constraint) (reverse (argument-types constraint)))

   (setf (gethash name (function-bindings *environment*))
         fn)))

(flet ((make-ftype (ret &rest args)
         (unless (consp ret)
           (setf ret (make-list (length args) :initial-element ret)))
         (apply #'mapcar
                (lambda (r &rest a)
                  (list a r))
                ret args)))
  (let* ((*environment* glsl::*glsl-base-environment*)
         (*global-environment* glsl::*glsl-base-environment*)
         ;; meta-types for defining the overloads
         (scalar (list :bool :int :uint :float :double))
         (vec (list :vec2 :vec3 :vec4))
         (ivec (list :ivec2 :ivec3 :ivec4))
         (uvec (list :uvec2 :uvec3 :uvec4))
         (bvec (list :bvec2 :bvec3 :bvec4))
         (dvec (list :dvec2 :dvec3 :dvec4))
         (mat (list :mat2 :mat3 :mat4 :mat2x3 :mat2x4 :mat3x2 :mat3x4 :mat4x3 :mat4x2))
         ;; for defining vector/matrix constructors
         ;;(2-vector (list :bvec2 :ivec2 :uvec2 :vec2 :dvec2))
         ;;(3-vector (list :bvec3 :ivec3 :uvec3 :vec3 :dvec3))
         ;;(4-vector (list :bvec4 :ivec4 :uvec4 :vec4 :dvec4))
         ;;(sqmat (list :mat2 :mat3 :mat4))
         (gen-type (cons :float vec))
         (gen-itype (cons :int ivec))
         (gen-utype (cons :uint uvec))
         (gen-dtype (cons :double dvec))
         ;; N scalars to simplify floatxvec and floatxmat signatures
         (fxv (make-list (length vec) :initial-element :float))
         (fxm (make-list (length mat) :initial-element :float))
         (ixv (make-list (length ivec) :initial-element :int))
         (uxv (make-list (length uvec) :initial-element :uint))
         ;; todo: decide if these should have signatures matching
         ;; implicit cats, or if those should be separate or not
         ;; available at all?
         (binop-args (make-ftype
                      (append gen-type gen-itype gen-utype
                              vec mat ivec vec mat ivec uvec uvec)
                      (append gen-type gen-itype gen-utype
                              vec mat ivec fxv fxm ixv uxv uvec)
                      (append gen-type gen-itype gen-utype
                              fxv fxm ixv vec mat ivec uvec uxv)))
         (unary-gentypes+mats (make-ftype
                               (append gen-type gen-itype gen-utype mat)
                               (append gen-type gen-itype gen-utype mat)))
         (scalar-compare (make-ftype
                          (list :bool :bool :bool)
                          (list :int :uint :float)
                          (list :int :uint :float)))
         (log* (make-ftype (append gen-itype gen-utype
                                   ivec ivec uvec uvec)
                           (append gen-itype gen-utype
                                   ivec ixv uvec uxv)
                           (append gen-itype gen-utype
                                   ixv ivec uxv uvec)
                           )))
    #++'(((:float :float) :float) ((:vec2 :vec2) :vec2)
         ((:vec3 :vec3) :vec3) ((:vec4 :vec4) :vec4)
         ((:mat2 :mat2) :mat2) ((:mat3 :mat3) :mat3)
         ((:mat4 :mat4) :mat4) ((:vec2 :mat2) :vec2)
         ((:vec3 :mat3) :vec3) ((:vec4 :mat4) :vec4)
         ((:mat2 :vec2) :vec2) ((:mat3 :vec3) :vec3)
         ((:mat4 :vec4) :vec4) ((:mat2 :float) :mat2)
         ((:mat3 :mat2) :mat3) ((:mat4 :mat3) :mat4)
         ((:float :mat4) :mat2) ((:vec2 :float) :mat3)
         ((:vec3 :vec2) :mat4) ((:vec4 :vec3) :vec2)
         ((:float :vec4) :vec3))
    ;;
    ;; these are all assumed to be binary at this point, any 0/1/n>2 -ary
    ;; uses should have been expanded to binary calls in earlier passes
    (add-internal-function/full '+ '(a b) binop-args)
    ;; fixme: verify the non-square matric types for *
    (add-internal-function/full '* '(a b)
                           ;;A right vector operand is treated as a
                           ;;column vector and a left vector operand as
                           ;;a row vector.
                           `( ;; 1xN Mx1 -> NxM
                             ;; -x-
                             ;; 2xN Mx2 -> MxN
                             ((:vec2 :vec2) :float)
                             ((:ivec2 :ivec2) :int)
                             ((:vec2 :mat2) :vec2)
                             ((:vec2 :mat3x2) :vec3)
                             ((:vec2 :mat4x2) :vec4)
                             ((:mat2 :vec2) :vec2)
                             ((:mat3x2 :vec2) :vec3)
                             ((:mat4x2 :vec2) :vec4)
                             ((:mat2 :mat2) :mat2)
                             ((:mat2 :mat3x2) :mat3x2)
                             ((:mat2 :mat4x2) :mat4x2)
                             ((:mat2x3 :mat2) :mat3x2)
                             ((:mat2x3 :mat3x2) :mat3)
                             ((:mat2x3 :mat4x2) :mat3x4)
                             ((:mat2x4 :mat2) :mat4x2)
                             ((:mat2x4 :mat3x2) :mat4x3)
                             ((:mat2x4 :mat4x2) :mat4)
                             ;; 3xN Mx3 -> MxN
                             ((:vec3 :vec3) :float)
                             ((:ivec3 :ivec3) :int)
                             ((:vec3 :mat2x3) :vec2)
                             ((:vec3 :mat3) :vec3)
                             ((:vec3 :mat4x3) :vec4)
                             ((:mat2x3 :vec3) :vec2)
                             ((:mat3 :vec3) :vec3)
                             ((:mat4x3 :vec3) :vec4)
                             ((:mat3x2 :mat2x3) :mat2)
                             ((:mat3x2 :mat3) :mat3x2)
                             ((:mat3x2 :mat4x3) :mat4x2)
                             ((:mat3 :mat2x3) :mat3x2)
                             ((:mat3 :mat3) :mat3)
                             ((:mat3 :mat4x3) :mat3x4)
                             ((:mat3x4 :mat2x3) :mat4x2)
                             ((:mat3x4 :mat3) :mat4x3)
                             ((:mat3x4 :mat4x3) :mat4)
                             ;; 4xN Mx4 -> MxN
                             ((:vec4 :vec4) :float)
                             ((:ivec4 :ivec4) :int)
                             ((:vec4 :mat2x4) :vec2)
                             ((:vec4 :mat3x4) :vec3)
                             ((:vec4 :mat4) :vec4)
                             ((:mat2x4 :vec4) :vec2)
                             ((:mat3x4 :vec4) :vec3)
                             ((:mat4 :vec4) :vec4)
                             ((:mat4x2 :mat2x4) :mat2)
                             ((:mat4x2 :mat3x4) :mat3x2)
                             ((:mat4x2 :mat4) :mat4x2)
                             ((:mat4x3 :mat2x4) :mat3x2)
                             ((:mat4x3 :mat3x4) :mat3)
                             ((:mat4x3 :mat4) :mat3x4)
                             ((:mat4 :mat2x4) :mat4x2)
                             ((:mat4 :mat3x4) :mat4x3)
                             ((:mat4 :mat4) :mat4)
                             ;; scalars
                             ((:int :int) :int)
                             ((:uint :uint) :uint)
                             ((:float :float) :float)
                             ,@(make-ftype (append vec mat ivec vec mat ivec)
                                           (append fxv fxm ixv vec mat ivec)
                                           (append vec mat ivec fxv fxm ixv))))
    ;; not sure if - should have a unary version or just print
    ;; (- 0 x) as -x ?
    ;; unary version is probably easier for type inference
    (add-internal-function/full '- '(a &optional b)
                           (append binop-args
                                   ;; unary version
                                   unary-gentypes+mats))
    ;; expanding (/ x) to (1/x) in printer as well, in hopes of simplifying
    ;; type inference
    (add-internal-function/full '/ '(a b)
                           (append binop-args
                                   unary-gentypes+mats))
    (add-internal-function/full 'mod '(a b) (make-ftype (append gen-itype gen-utype
                                                           ivec uvec ivec uvec)
                                                   (append gen-itype gen-utype
                                                           ivec uvec ixv uxv)
                                                   (append gen-itype gen-utype
                                                           ixv uxv ivec uvec)))
    ;;
    (add-internal-function/full 'or '(a b) '(((:bool :bool) :bool)))
    (add-internal-function/full 'and '(a b) '(((:bool :bool) :bool)))
    ;; is this glsl '!' or glsl 'not' or both?
    ;; assuming we can get type info during printing to pick right one...
    (add-internal-function/full 'not '(a) (append
                                      '(((:bool) :bool))
                                      (make-ftype bvec bvec)))
    (add-internal-function/full 'logior '(a b) log*)
    (add-internal-function/full 'logand '(a b) log*)
    (add-internal-function/full 'logxor '(a b) log*)
    ;; args like (T x) will be constrained to same type as arg X
    (add-internal-function/s '= '(a b) '(T (= 0)) :bool)
    (add-internal-function/s '/= '(a b) '(T (= 0)) :bool)
    ;; should these work on vectors etc too?
    ;; (would need to be able to see types in printer to expand to
    ;;  lessThan etc)
    (add-internal-function/full '< '(a b) scalar-compare)
    (add-internal-function/full '> '(a b) scalar-compare)
    (add-internal-function/full '<= '(a b) scalar-compare)
    (add-internal-function/full '>= '(a b) scalar-compare)
    ;; including 1+ and 1- to simplify type inference, so we don't have
    ;; to know what type of 1 to use
    (add-internal-function/full '1- '(number) unary-gentypes+mats)
    (add-internal-function/full '1+ '(number) unary-gentypes+mats)
    ;; todo ++, --, ash
    ;;(add-internal-function/full 'ash '(integer count))


    ;; glsl specific things
    (add-internal-function/full 'glsl::^^ '(a b) '(((:bool :bool) :bool)))
    ;; fixme: verify types for <<
    ;; possibly also simplify?
    ;;  return is same type as first arg
    ;;  2nd arg is same size vec or scalar, but doesn't have to match
    ;;    signed vs unsigned
    (add-internal-function/full 'glsl::<< '(integer count)
                                `(((:int :int) :int)
                                  ((:int :uint) :int)
                                  ((:uint :int) :uint)
                                  ((:uint :uint) :uint)
                                  ,@(make-ftype (append ivec uvec ivec uvec
                                                        ivec uvec ivec uvec)
                                                (append ivec uvec ivec uvec
                                                        ivec uvec ivec uvec)
                                                (append ivec uvec uvec ivec
                                                        ixv uxv uxv ixv))))
    (add-internal-function/full 'glsl::>> '(integer count)
                                `(((:int :int) :int)
                                  ((:int :uint) :int)
                                  ((:uint :int) :uint)
                                  ((:uint :uint) :uint)
                                  ,@(make-ftype (append ivec uvec ivec uvec
                                                        ivec uvec ivec uvec)
                                                (append ivec uvec ivec uvec
                                                        ivec uvec ivec uvec)
                                                (append ivec uvec uvec ivec
                                                        ixv uxv uxv ixv))))
    (add-internal-function/s 'return '(value)
                             '(t) '(= 0))

    ;; fixme: most of these belong in glsl: package
    (macrolet ((add/s (&rest definitions)
                 `(progn
                    ,@(loop for (.name lambda-list arg-types return-type)
                              in definitions
                            for (name glsl-name . keys)
                              = (alexandria:ensure-list .name)
                            collect `(add-internal-function/s ',name
                                                              ',lambda-list
                                                              ,arg-types
                                                              ,return-type
                                                              :glsl-name
                                                              ,glsl-name
                                                              ,@keys))))
               (add/f (&rest definitions)
                 `(progn
                    ,@(loop for (.name lambda-list ftype)
                              in definitions
                            for (name glsl-name . keys)
                              = (alexandria:ensure-list .name)
                            collect `(add-internal-function/full ',name
                                                                 ',lambda-list
                                                                 ,ftype
                                                                 :glsl-name
                                                                 ,glsl-name
                                                                 ,@keys))))
               (add/f1 (&rest definitions)
                 `(progn
                    ,@(loop for (.name lambda-list args ret)
                              in definitions
                            for (name glsl-name . keys)
                              = (alexandria:ensure-list .name)
                            for ftype = `(mapcar (lambda (a) (list a ,ret))
                                                 ,args)
                            collect `(add-internal-function/full ',name
                                                                 ',lambda-list
                                                                 ,ftype
                                                                 :glsl-name
                                                                 ,glsl-name
                                                                 ,@keys))))
               (add/m (&rest definitions)
                 `(progn
                    ,@(loop for (.name lambda-list count ret)
                              in definitions
                            for (name glsl-name) = (alexandria:ensure-list
                                                    .name)
                            collect `(add-internal-function/mat ',name
                                                                 ',lambda-list
                                                                 ,count
                                                                 ,ret
                                                                 :glsl-name
                                                                 ,glsl-name)))))
      (add/s
       ;; todo: check specs and update these types...
       ((texture-2d "texture2D") (sampler uv) `(:sampler-2d ,(cons 'or vec)
                                                :float) :vec4)
       (texture (sampler uv bias) `(:sampler-2d ,(cons 'or vec) :float) :vec4)
       (texel-fetch (sampler texcoord lod sample) `(:sampler-2d ,(cons 'or ivec)
                                                    :int :int) :vec4)
       (normalize (vec) `(,(cons 'or vec)) '(= 0))
       (length (vec) `(,(cons 'or vec)) :float)
       (sign (x) `(,(cons 'or gen-type)) '(= 0))
       (min (a b) '(:float :float) :float)
       (max (a b) '(:float :float) :float)
       (dot (a b) `(,(cons 'or vec) (= 0)) :float)
       (abs (a) '(:float) :float)
       (sqrt (a) '(:float) :float)
       (pow (a b) '(:float :float) :float)
       (exp (a) '(:float) :float)
       (exp2 (a) '(:float) :float)
       (floor (a) `(,(cons 'or gen-type)) '(= 0))
       (fract (a) `(,(cons 'or gen-type)) '(= 0))
       (less-than (a b) `(,(cons 'or gen-type) (= 0)) '(= 0))
       ;; GLSL 'step' instead of CL STEP
       (step (edge x) `((=s 1) ,(cons 'or gen-type)) '(= 1))
       (clamp (a b) `(,(cons 'or gen-type) (=s 0)) '(= 0))
       (noise4 (a) `(,(cons 'or gen-type)) :vec4)
       (cross (a b) `(,(cons 'or vec) (= 0)) '(= 0))
       ((emit-vertex "EmitVertex") () () :void)
       ((end-primitive "EndPrimitive") () () :void)
       (reflect (a b) '(:vec3 :vec3) :vec3)
       ;; not completely sure about this signature...
       ;; genType,genType,genType->genType
       ;; float,float,genType->genType
       ;; same for genDType and double
       ((smooth-step "smoothstep") (edge0 edge1 x) `((=s 2)
                                                     (=s 0)
                                                     ,(cons 'or
                                                            (append gen-type
                                                                    gen-dtype)))
        '(= 2))
       (any (x) `(,(cons 'or bvec)) :bool)
       (all (x) `(,(cons 'or bvec)) :bool)
       #++(not (x) :bvec (:bvec))
       ;; fixme: add constraint for same size vectors of different types?
       (equal (x y) `(,(cons 'or (append vec ivec uvec bvec dvec)) (= 0))
              `(=# 0 :bool))
       (not-equal (x y) `(,(cons 'or vec) (= 0))
                  `(=# 0 :bool))
       (less-than (x y) `(,(cons 'or vec) (= 0))
                  `(=# 0 :bool))
       (less-than-equal (x y) `(,(cons 'or vec) (= 0))
                        `(=# 0 :bool))
       (greater-than (x y) `(,(cons 'or vec) (= 0))
                     `(=# 0 :bool))
       (greater-than-equal (x y) `(,(cons 'or vec) (= 0))
                           `(=# 0 :bool))
       ;; not completely sure if mat is allowed here?
       ;; might also allow arrays?
       (int (x) `((or :int :uint :bool :float
                   :double ,@ivec ,@uvec ,@vec ,@dvec ,@mat)) :int)
       (uint (x) `((or :int :uint :bool :float
                    :double  ,@ivec ,@uvec ,@vec ,@dvec ,@mat)) :uint)
       (bool (x) `((or :int :uint :bool :float
                    :double ,@ivec ,@uvec ,@vec ,@dvec ,@mat)) :bool)
       (float (x) `((or :int :uint :bool :float
                     :double ,@ivec ,@uvec ,@vec ,@dvec ,@mat)) :float)
       (double (x) `((or :int :uint :bool :float
                      :double ,@ivec ,@uvec ,@vec ,@dvec ,@mat)) :double)
)

      (labels ((vec/mat-constructor (n &optional (base :float))
                 (let ((foo '((:bool ((:bvec2 2) (:bvec3 3) (:bvec4 4)))
                              (:int ((:ivec2 2) (:ivec3 3) (:ivec4 4)))
                              (:uint ((:uvec2 2) (:uvec3 3) (:uvec4 4)))
                              (:float ((:vec2 2) (:vec3 3) (:vec4 4)))
                              (:double ((:dvec2 2) (:dvec3 3) (:dvec4 4))))))
                   (if (zerop n)
                       (list nil)
                       (loop for (type count) in
                             `(,@(mapcar (lambda (a) (list a 1)) scalar)
                               ,@(cadr (assoc base foo)))
                             when (<= count n)
                               append (mapcar (lambda (a) (cons type a))
                                              (vec/mat-constructor (- n count)
                                                                   base)
                                              ))))))

        #++(add/f1
                ((glsl::bvec2 nil :allow-casts nil) (a &optional b)
                 (vec/mat-constructor 2 :bool) :bvec2)
                ((glsl::bvec3 nil :allow-casts nil) (a &optional b c)
                 (vec/mat-constructor 3 :bool) :bvec3)
                ((glsl::bvec4 nil :allow-casts nil) (a &optional b c d)
                 (append '((:mat2)) (vec/mat-constructor 4 :bool)) :bvec4)

                ((glsl::ivec2 nil :allow-casts nil) (a &optional b)
                 (vec/mat-constructor 2 :int) :ivec2)
                ((glsl::ivec3 nil :allow-casts nil) (a &optional b c)
                 (vec/mat-constructor 3 :int) :ivec3)
                ((glsl::ivec4 nil :allow-casts nil) (a &optional b c d)
                 (append '((:mat2)) (vec/mat-constructor 4 :int)) :ivec4)

                ((glsl::uvec2 nil :allow-casts nil) (a &optional b)
                 (vec/mat-constructor 2 :uint) :uvec2)
                ((glsl::uvec3 nil :allow-casts nil) (a &optional b c)
                 (vec/mat-constructor 3 :uint) :uvec3)
                ((glsl::uvec4 nil :allow-casts nil) (a &optional b c d)
                 (append '((:mat2)) (vec/mat-constructor 4 :uint)) :uvec4)

                ((glsl::vec2 nil :allow-casts nil) (a &optional b)
                 (vec/mat-constructor 2 :float) :vec2)
                ((glsl::vec3 nil :allow-casts nil) (a &optional b c)
                 (vec/mat-constructor 3 :float) :vec3)
                ((glsl::vec4 nil :allow-casts nil) (a &optional b c d)
                 (append '((:mat2)) (vec/mat-constructor 4 :float)) :vec4)

                ((glsl::dvec2 nil :allow-casts nil) (a &optional b)
                 (vec/mat-constructor 2 :double) :dvec2)
                ((glsl::dvec3 nil :allow-casts nil) (a &optional b c)
                 (vec/mat-constructor 3 :double) :dvec3)
                ((glsl::dvec4 nil :allow-casts nil) (a &optional b c d)
                 (append '((:mat2)) (vec/mat-constructor 4 :double)) :dvec4))


        (add/m
         (glsl::bvec2 (a &optional b) 2 :bvec2)
         (glsl::bvec3 (a &optional b c) 3 :bvec3)
         (glsl::bvec4 (a &optional b c d) 4 :bvec4)

         (glsl::ivec2 (a &optional b) 2 :ivec2)
         (glsl::ivec3 (a &optional b c) 3 :ivec3)
         (glsl::ivec4 (a &optional b c d) 4 :ivec4)

         (glsl::uvec2 (a &optional b) 2 :uvec2)
         (glsl::uvec3 (a &optional b c) 3 :uvec3)
         (glsl::uvec4 (a &optional b c d) 4 :uvec4)

         (glsl::vec2 (a &optional b) 2 :vec2)
         (glsl::vec3 (a &optional b c) 3 :vec3)
         (glsl::vec4 (a &optional b c d) 4 :vec4)

         (glsl::dvec2 (a &optional b) 2 :dvec2)
         (glsl::dvec3 (a &optional b c) 3 :dvec3)
         (glsl::dvec4 (a &optional b c d) 4 :dvec4)

         (glsl::mat2 (a &optional b c d) 4 :mat2)
         (glsl::mat2x3 (a &optional b c d e f) 6 :mat2x3)
         (glsl::mat2x4 (a &optional b c d e f g h) 8 :mat2x4)
         (glsl::mat3x2 (a &optional b c d e f) 6 :mat3x2)
         (glsl::mat3 (a &optional b c d e f g h i) 9  :mat3)
         (glsl::mat3x4 (a &optional b c d e f g h i j k l) 12  :mat3x4)
         (glsl::mat4x2 (a &optional b c d e f g h) 8  :mat4x2)
         (glsl::mat4x3 (a &optional b c d e f g h i j k l) 12 :mat4x3)
         (glsl::mat4 (a &optional b c d e f g h i j k l m n o p) 16 :mat4))
                ;; todo :dmat*


        (add-internal-function/full 'transpose '(x) (make-ftype
                                                     '(:mat2 :mat3 :mat4
                                                       :mat2x3 :mat2x4
                                                       :mat3x2 :mat3x4
                                                       :mat4x2 :mat4x3)
                                                     '(:mat2 :mat3 :mat4
                                                       :mat3x2 :mat4x2
                                                       :mat2x3 :mat4x3
                                                       :mat2x4 :mat3x4))))


)
      )



    )

;; (defmacro add-binop (name  name.2 &optional default)
;;   `(defclmacro ,name (&rest args)
;;      (if (and ,default (< (length args) 2))
;;          `(,',name.2 ,',default ,(first args))
;;          (loop with (a b) = args
;;                with form = (list ',name.2 a b)
;;                for c in (cddr args)
;;                while c
;;                do (setf form (list ',name.2 form c))
;;                finally (return form)))))
;;
;;
;; (add-binop + |+.2| 0)
;; (add-binop - |-.2| 0)
;; (add-binop * *.2 1)
;; (add-binop / /.2 1)
;; (add-binop = =.2)
;; (add-binop > >.2)
;; (add-binop >= >=.2)
;; (add-binop cons cons.2)
;;
;; (let ((vec (list :vec2 :vec3 :vec4))
;;       (ivec (list :ivec2 :ivec3 :ivec4))
;;       (bvec (list :bvec2 :bvec3 :bvec4))
;;       (mat (list :mat2 :mat3 :mat4))
;;       (gen-type (list :float :vec2 :vec3 :vec4))
;;       (fff (list :float)))
;;   (mapcar (lambda (a b c) (list (list a b) c))
;;           (append gen-type mat vec mat mat fff vec fff)
;;           (append gen-type mat mat vec fff mat fff vec)
;;           (append gen-type mat vec vec mat mat vec vec)))


#++
(flet ((make-ftype (ret &rest args)
         (apply #'mapcar (lambda (r &rest a)
                  (format t "~s -> ~s~%" a r)
                  (list a r))
                ret args)))
 (let* ((*environment* *cl-environment*)
        (*global-environment* *cl-environment*)
         ;; meta-types for defining the overloads
         (vec (list :vec2 :vec3 :vec4))
         (ivec (list :ivec2 :ivec3 :ivec4))
         (uvec (list :uvec2 :uvec3 :uvec4))
         (bvec (list :bvec2 :bvec3 :bvec4))
         (mat (list :mat2 :mat3 :mat4 :mat2x3 :mat2x4 :mat3x2 :mat3x4 :mat4x3 :mat4x2))
         (sqmat (list :mat2 :mat3 :mat4))
         (gen-type (cons :float vec))
         (gen-itype (cons :int ivec))
         (gen-utype (cons :uint uvec))
        ;; 3 scalars to simplify floatxvec and floatxmat signatures
        (fxv (make-list (length vec) :initial-element :float))
        (fxm (make-list (length mat) :initial-element :float))
        (ixv (make-list (length ivec) :initial-element :int))
        (uxv (make-list (length uvec) :initial-element :uint))
        ;; todo: decide if these should have signatures matching
        ;; implicit cats, or if those should be separate or not
        ;; available at all?
        (binop-args (make-ftype
                     (append gen-type gen-itype gen-utype
                             vec mat ivec vec mat ivec uvec uvec)
                     (append gen-type gen-itype gen-utype
                             vec mat ivec fxv fxm ixv uxv uvec)
                     (append gen-type gen-itype gen-utype
                             fxv fxm ixv vec mat ivec uvec uxv))))
   (format t "~:{~(~s -> ~s~)~%~}"
                          ;;A right vector operand is treated as a
                          ;;column vector and a left vector operand as
                          ;;a row vector.
                          `(;; 1xN Mx1 -> NxM
                            ;; -x-
                            ;; 2xN Mx2 -> MxN
                            ((:vec2 :vec2) :float)
                            ((:ivec2 :ivec2) :int)
                            ((:vec2 :mat2) :vec2)
                            ((:vec2 :mat3x2) :vec3)
                            ((:vec2 :mat4x2) :vec4)
                            ((:mat2 :vec2) :vec2)
                            ((:mat3x2 :vec2) :vec3)
                            ((:mat4x2 :vec2) :vec4)
                            ((:mat2 :mat2) :mat2)
                            ((:mat2 :mat3x2) :mat3x2)
                            ((:mat2 :mat4x2) :mat4x2)
                            ((:mat2x3 :mat2) :mat3x2)
                            ((:mat2x3 :mat3x2) :mat3)
                            ((:mat2x3 :mat4x2) :mat3x4)
                            ((:mat2x4 :mat2) :mat4x2)
                            ((:mat2x4 :mat3x2) :mat4x3)
                            ((:mat2x4 :mat4x2) :mat4)
                            ;; 3xN Mx3 -> MxN
                            ((:vec3 :vec3) :float)
                            ((:ivec3 :ivec3) :int)
                            ((:vec3 :mat2x3) :vec2)
                            ((:vec3 :mat3) :vec3)
                            ((:vec3 :mat4x3) :vec4)
                            ((:mat2x3 :vec3) :vec2)
                            ((:mat3 :vec3) :vec3)
                            ((:mat4x3 :vec3) :vec4)
                            ((:mat3x2 :mat2x3) :mat2)
                            ((:mat3x2 :mat3) :mat3x2)
                            ((:mat3x2 :mat4x3) :mat4x2)
                            ((:mat3 :mat2x3) :mat3x2)
                            ((:mat3 :mat3) :mat3)
                            ((:mat3 :mat4x3) :mat3x4)
                            ((:mat3x4 :mat2x3) :mat4x2)
                            ((:mat3x4 :mat3) :mat4x3)
                            ((:mat3x4 :mat4x3) :mat4)
                            ;; 4xN Mx4 -> MxN
                            ((:vec4 :vec4) :float)
                            ((:ivec4 :ivec4) :int)
                            ((:vec4 :mat2x4) :vec2)
                            ((:vec4 :mat3x4) :vec3)
                            ((:vec4 :mat4) :vec4)
                            ((:mat2x4 :vec4) :vec2)
                            ((:mat3x4 :vec4) :vec3)
                            ((:mat4 :vec4) :vec4)
                            ((:mat4x2 :mat2x4) :mat2)
                            ((:mat4x2 :mat3x4) :mat3x2)
                            ((:mat4x2 :mat4) :mat4x2)
                            ((:mat4x3 :mat2x4) :mat3x2)
                            ((:mat4x3 :mat3x4) :mat3)
                            ((:mat4x3 :mat4) :mat3x4)
                            ((:mat4 :mat2x4) :mat4x2)
                            ((:mat4 :mat3x4) :mat4x3)
                            ((:mat4 :mat4) :mat4)
                            ;; scalars
                            ((:int :int) :int)
                            ((:uint :uint) :uint)
                            ((:float :float) :float)
                            ,@(make-ftype (append vec mat ivec vec mat ivec)
                                           (append fxv fxm ixv vec mat ivec)
                                           (append vec mat ivec fxv fxm ixv)
                                          )

                            )

)
))
