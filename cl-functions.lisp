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
)

(defun add-internal-function/mat (name lambda-list count arg-types return-type
                                  &key glsl-name)
  ;; matrix constructors types are too complex to enumerate explicitly
  ;; (nearly 3 million for mat3), so we need to use a special
  ;; constraint for them
  ;; (may just accept 1-COUNT of ARG-TYPES for now, eventually should
  ;;  enforce that it is passed either 1 matrix type or a combination
  ;;  of arg-types that adds up to exactly COUNT elements)

)
(defun add-internal-function/full (name lambda-list type &key glsl-name (allow-casts t))
  (let* ((fn (make-instance 'internal-function
                            :name name
                            :glsl-name glsl-name
                            :lambda-list lambda-list
                            :declared-type type))
         (constraint (make-instance
                      'function-application
                      :name name
                      :function-types type))
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
    (setf (return-type constraint) ret
          (value-type fn) ret)

    ;; allowing &optional in lambda list, no other l-l-keywords though
    ;; just plain symbols for optional args, no default or -p arg
    (flet ((make-type (n)
             (let ((type (make-instance 'constrained-type
                                        :types
                                        (alexandria:alist-hash-table
                                         (mapcar (lambda (a)
                                                   (cons (or (get-type-binding a)
                                                             a
                                                             nil)
                                                         t))
                                                 (delete-duplicates
                                                  (mapcar (lambda (a)
                                                            (nth n (car a)))
                                                          type))))
                                        :constraints
                                        (alexandria:plist-hash-table
                                         (list constraint t)))))
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
         (igen-type (cons :int ivec))
         (ugen-type (cons :uint uvec))
         ;; N scalars to simplify floatxvec and floatxmat signatures
         (fxv (make-list (length vec) :initial-element :float))
         (fxm (make-list (length mat) :initial-element :float))
         (ixv (make-list (length ivec) :initial-element :int))
         (uxv (make-list (length uvec) :initial-element :uint))
         ;; todo: decide if these should have signatures matching
         ;; implicit cats, or if those should be separate or not
         ;; available at all?
         (binop-args (make-ftype
                      (append gen-type igen-type ugen-type
                              vec mat ivec vec mat ivec uvec uvec)
                      (append gen-type igen-type ugen-type
                              vec mat ivec fxv fxm ixv uxv uvec)
                      (append gen-type igen-type ugen-type
                              fxv fxm ixv vec mat ivec uvec uxv)))
         (unary-gentypes+mats (make-ftype
                               (append gen-type igen-type ugen-type mat)
                               (append gen-type igen-type ugen-type mat)))
         (scalar-compare (make-ftype
                          (list :bool :bool :bool)
                          (list :int :uint :float)
                          (list :int :uint :float)))
         (log* (make-ftype (append igen-type ugen-type
                                   ivec ivec uvec uvec)
                           (append igen-type ugen-type
                                   ivec ixv uvec uxv)
                           (append igen-type ugen-type
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
    (add-internal-function/full 'mod '(a b) (make-ftype (append igen-type ugen-type
                                                           ivec uvec ivec uvec)
                                                   (append igen-type ugen-type
                                                           ivec uvec ixv uxv)
                                                   (append igen-type ugen-type
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
    (add-internal-function/s '= '(a b) '(T (T 0)) :bool)
    (add-internal-function/s '/= '(a b) '(T (T 0)) :bool)
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
                             '(t) '(t 0))

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
                    ,@(loop for (.name lambda-list count arg-type ret)
                              in definitions
                            for (name glsl-name) = (alexandria:ensure-list
                                                    .name)
                            collect `(add-internal-function/mat ',name
                                                                 ',lambda-list
                                                                 ,count
                                                                 ,arg-type
                                                                 ,ret
                                                                 :glsl-name
                                                                 ,glsl-name)))))
      (add/s
       ;; todo: check specs and update these types...
       ((texture-2d "texture2D") (sampler uv) `(:sampler2D ,vec :float) :vec4)
       (texture (sampler uv bias) `(:sampler2D ,vec :float) :vec4)
       (texel-fetch (sampler texcoord lod sample) `(:sampler2D ,ivec :int :int) :vec4)
       (normalize (vec) '(:vec) '(t 0))
       (length (vec) '(:vec) :float)
       (sign (x) `(,gen-type) '(t 0))
       (min (a b) '(:float :float) :float)
       (max (a b) '(:float :float) :float)
       (dot (a b) `(,vec (t 0)) :float)
       (abs (a) '(:float) :float)
       (sqrt (a) '(:float) :float)
       (pow (a b) '(:float :float) :float)
       (exp (a) '(:float) :float)
       (exp2 (a) '(:float) :float)
       (floor (a) `(,gen-type) '(t 0))
       (fract (a) `(,gen-type) '(t 0))
       (less-than (a b) `(,gen-type (t 0)) '(t 0))
       ;; GLSL 'step' instead of CL STEP
       (step (x a b) `(,gen-type (t 0) (t 0)) '(t 0))
       (clamp (a b) `(,gen-type (t 0)) '(t 0))
       (noise4 (a) `(,gen-type) :vec4)
       (cross (a b) `(,vec (t 0)) '(t 0))
       ((emit-vertex "EmitVertex") () () :void)
       ((end-primitive "EndPrimitive") () () :void)
       (reflect (a b) '(:vec3 :vec3) :vec3)
       ((smooth-step "smoothstep") (edge0 edge1 x) `(,gen-type (t 0) :float) '(t 0))
       (any (x) `(,bvec) :bool)
       (all (x) `(,bvec) :bool)
       #++(not (x) :bvec (:bvec))
       ;; fixme: add constraint for same size vectors of different types?
       (equal (x y) `(,vec (t 0)) bvec)
       (not-equal (x y) `(,vec (t 0)) bvec)
       (less-than (x y) `(,vec (t 0)) bvec)
       (less-than-equal (x y) `(,vec (t 0)) bvec)
       (greater-than (x y) `(,vec (t 0)) bvec)
       (greater-than-equal (x y) `(,vec (t 0)) bvec)
       ;; not completely sure if mat is allowed here?
       ;; might also allow arrays?
       (int (x) `((:int :uint :bool :float
                   :double ,ivec ,uvec ,vec ,dvec ,mat)) :int)
       (uint (x) `((:int :uint :bool :float
                    :double  ,ivec ,uvec ,vec ,dvec ,mat)) :uint)
       (bool (x) `((:int :uint :bool :float
                    :double ,ivec ,uvec ,vec ,dvec ,mat)) :bool)
       (float (x) `((:int :uint :bool :float
                     :double ,ivec ,uvec ,vec ,dvec ,mat)) :float)
       (double (x) `((:int :uint :bool :float
                      :double ,ivec ,uvec ,vec ,dvec ,mat)) :double)
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

        (add/f1
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
         (glsl::mat2 (a &optional b c d)
                     4 (append scalar vec mat) :mat2)

         (glsl::mat2x3 (a &optional b c d e f)
                       6  (append scalar vec mat) :mat2x3)
         (glsl::mat2x4 (a &optional b c d e f g h)
                       8 (append scalar vec mat) :mat2x4)
         (glsl::mat3x2 (a &optional b c d e f)
                       6 (append scalar vec mat) :mat3x2)
         (glsl::mat3 (a &optional b c d e f g h i)
                     9 (append scalar vec mat) :mat3)
         (glsl::mat3x4 (a &optional b c d e f g h i j k l)
                 12 (append scalar vec mat) :mat3x4)
         (glsl::mat4x2 (a &optional b c d e f g h)
                 8 (append scalar vec mat) :mat4x2)
         (glsl::mat4x3 (a &optional b c d e f g h i j k l)
                 12 (append scalar vec mat) :mat4x3)
         (glsl::mat4 (a &optional b c d e f g h i j k l m n)
               16 (append scalar vec mat) :mat4))
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
         (igen-type (cons :int ivec))
         (ugen-type (cons :uint uvec))
        ;; 3 scalars to simplify floatxvec and floatxmat signatures
        (fxv (make-list (length vec) :initial-element :float))
        (fxm (make-list (length mat) :initial-element :float))
        (ixv (make-list (length ivec) :initial-element :int))
        (uxv (make-list (length uvec) :initial-element :uint))
        ;; todo: decide if these should have signatures matching
        ;; implicit cats, or if those should be separate or not
        ;; available at all?
        (binop-args (make-ftype
                     (append gen-type igen-type ugen-type
                             vec mat ivec vec mat ivec uvec uvec)
                     (append gen-type igen-type ugen-type
                             vec mat ivec fxv fxm ixv uxv uvec)
                     (append gen-type igen-type ugen-type
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
