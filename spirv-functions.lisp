(in-package #:3bgl-shaders)

(defparameter *current-call* nil)

(defmethod dump-internal-call (name args)
  (cerror "continue" "don't know how to dump internal function call ~s? (~s)"
          name *current-call*))

(defmethod dump-spirv-call (f call args)
  (cerror "continue" "don't know how to dump function call ~s?" f))

(defmethod dump-spirv-call ((f internal-function) call args)
  (let ((*current-call* call))
    (dump-internal-call (name f) args)))

(defmacro defint (name (&rest lambda-list) &body body)
  (alexandria:with-gensyms (args)
    `(defmethod dump-internal-call ((name (eql ',name)) ,args)
       (destructuring-bind (,@lambda-list) ,args
         ,@body))))

(defint 3bgl-glsl:barrier ()
  (add-spirv `(spirv-core:control-barrier :workgroup :device ()))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *type-abbreviations*
    ;; simple expansions for now, add/refactor as needed
    (alexandria:plist-hash-table
     '(integral (:uint8 :int8 :uint16 :int16 :uint :int :uint64 :int64)
       integral-vector (:u8vec2 :u8vec3 :u8vec4 :i8vec2 :i8vec3 :i8vec4
                        :u16vec2 :u16vec3 :u16vec4 :i16vec2 :i16vec3 :i16vec4
                        :uvec2 :uvec3 :uvec4 :ivec2 :ivec3 :ivec4
                        :u64vec2 :u64vec3 :u64vec4 :i64vec2 :i64vec3 :i64vec4)
       signed (:int8 :int16 :int :int64)
       signed-vector (:i8vec2 :i8vec3 :i8vec4 :i16vec2 :i16vec3 :i16vec4
                      :ivec2 :ivec3 :ivec4 :i64vec2 :i64vec3 :i64vec4)

       unsigned (:uint8 :uint16 :uint :uint64)
       unsigned-vector (:u8vec2 :u8vec3 :u8vec4 :u16vec2 :u16vec3 :u16vec4
                        :uvec2 :uvec3 :uvec4 :u64vec2 :u64vec3 :u64vec4)

       floating (:float16 :float :double)
       floating-vector (:f16vec2 :f16vec3 :f16vec4 :vec2 :vec3 :vec4
                        :dvec2 :dvec3 :dvec4)
       matrix (:f16mat2 :f16mat3 :f16mat4 :f16mat2x3 :f16mat2x4 :f16mat3x2
               :f16mat3x4 :f16mat4x2 :f16mat4x3 :mat2 :mat3 :mat4
               :mat2x3 :mat2x4 :mat3x2 :mat3x4 :mat4x2 :mat4x3
               :dmat2 :dmat3 :dmat4 :dmat2x3 :dmat2x4 :dmat3x2
               :dmat3x4 :dmat4x2 :dmat4x3)
       bool-vector (:bvec2 :bvec3 :bvec4)
       nil (nil))))

  (defun expand-type (type)
    (or (gethash type *type-abbreviations*)
        (alexandria:ensure-list type)))

  (defun expand-type-set (types)
    ;; recursively expand a type or list of type of glsl type abbreviations
    (let* ((types (remove-duplicates (alexandria:ensure-list types)))
           (expanded (remove-duplicates
                      (loop for i in types append (expand-type i)))))
      (if (alexandria:set-equal types expanded)
          expanded
          (expand-type-set expanded)))))

(defmacro with-matcher (name (type1 &optional (type2 nil type2p)) &body body)
  (alexandria:once-only (type1 type2)
    `(macrolet ((,name (set1 ,@(when type2p '(set2)))
                  (let ((set1 (expand-type-set set1))
                        ,@(when type2p
                            '((set2 (expand-type-set set2)))))
                    `(progn
                       (format t "~&matching: ~s in ~s~%           ~s in ~s~%"
                               ,',type1 ',set1
                               ,',type2 ',,(when type2p 'set2))
                       ,,(if (not type2p)
                             ``(member ,',type1 ',set1)
                             `(if set2
                                  `(and (member ,',type1 ',set1
                                                )
                                        (member ,',type2 ',set2))
                                  `(and (member ,',type1 ',set1)
                                        (eql ,',type2 nil))))))))
       ,@body)))

(defun vcast (t1 t2 v)
  (let* ((tmp (spv-tmp))
         (size (scalar/vector-size t2))
         (cast-type (aref (scalar/vector-set t1) size))
         (cast-name (name cast-type)))
    (add-spirv `(spirv-core:composite-construct
                 ,tmp ,cast-name ,@ (loop repeat size
                                          collect v)))
    (values tmp cast-name cast-type)))
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun ll-vars (x)
   ;; todo: error on &key/&rest (or handle them if needed)
   (multiple-value-bind (r o) (alexandria:parse-ordinary-lambda-list x)
     (append r (mapcar 'first o)))))

(defmacro defint* (name (&rest lambda-list) &body body)
  ;; seeing a bunch of verbose common parts, so wrapping it up in a macro
  ;; binds RET, TYPES, RTYPE, TYPE1, .TYPE1 (and TYPE2, .TYPE2 if needed)
  ;; automatically returns RET
  ;; adds macro S->V-CASTS to handle casting a scalar in either
  ;; argument to vector of appropriate type (probably will need more
  ;; variants for different definitions of 'appropriate')
  (let* ((args (ll-vars lambda-list))
         (type-vars (subseq '(.type1 .type2) 0 (length args)))
         (type-name-vars (subseq '(type1 type2) 0 (length args))))
    `(defint ,name ,lambda-list
       (macrolet ((s->v-casts (&rest scalar-vector-type-pairs)
                    ;; most casts are handled by caller, but for some ops
                    ;; we want to be able to use scalar with vector even
                    ;; if there is no op for that, so add a manual cast
                    ;; if needed
                    `(with-matcher m (,@',type-name-vars)
                       (cond
                         ((or ,@(loop for (s v) in scalar-vector-type-pairs
                                      collect `(m ,s ,v)))
                          (setf (values ,',(first args)
                                        ,',(first type-name-vars)
                                        ,',(first type-vars))
                                (vcast (second types) (third types)
                                       ,',(first args))))
                         ((or ,@(loop for (s v) in scalar-vector-type-pairs
                                      collect `(m ,v ,s)))
                          (setf (values ,',(second args)
                                        ,',(second type-name-vars)
                                        ,',(second type-vars))
                                (vcast (third types) (second types)
                                       ,',(second args))))))))
         (let* ((ret (spv-tmp))
                (types (gethash *current-call* *binding-types*))
                (rtype (name (first types)))
                ,@(loop for type in type-vars
                        for name in type-name-vars
                        for n from 1
                        collect  `(,type (nth ,n types))
                        collect `(,name (name ,type))))
           ,@body
           ret)))))

(defint* * (a b)
  ;; we only need scalar->vector casts for ints
  (s->v-casts (integral integral-vector))
  ;; if int*int or ivec*ivec use i-mul
  ;; if float*float or fvec*fvec use f-mul
  ;; if fvec * float or float * fvec use vector-times-scalar
  ;; if mat * float or float * mat use matrix-times-scalar
  ;; if mat * fvec use matrix-times-vector
  ;; if fvec * mat use vector-times-matrix
  ;; if mat * mat use matrix-times-matrix
  (with-matcher m (type1 type2)
    (cond
      ((or (m integral integral)
           (m integral-vector integral-vector))
       (add-spirv `(spirv-core:i-mul ,ret ,rtype ,a ,b)))
      ((or (m floating floating)
           (m floating-vector floating-vector))
       (add-spirv `(spirv-core:f-mul ,ret ,rtype ,a ,b)))
      ((m floating-vector floating)
       (add-spirv `(spirv-core:vector-times-scalar ,ret ,rtype ,a ,b)))
      ((m floating floating-vector)
       (add-spirv `(spirv-core:vector-times-scalar ,ret ,rtype ,b ,a)))
      ((m matrix floating)
       (add-spirv `(spirv-core:matrix-times-scalar ,ret ,rtype ,a ,b)))
      ((m floating matrix)
       (add-spirv `(spirv-core:matrix-times-scalar ,ret ,rtype ,b ,a)))
      ((m matrix floating-vector)
       (add-spirv `(spirv-core:matrix-times-vector ,ret ,rtype ,a ,b)))
      ((m floating-vector matrix)
       (add-spirv `(spirv-core:vector-times-matrix ,ret ,rtype ,a ,b)))
      ((m matrix matrix)
       (add-spirv `(spirv-core:matrix-times-matrix ,ret ,rtype ,a ,b)))
      (t (error "can't multiply ~s by ~s" type1 type2)))))

(defint* + (a b)
  (s->v-casts (integral integral-vector)
              (floating floating-vector))
  ;; if int+int or ivec+ivec use i-add
  ;; if float+float or fvec+fvec use f-add
  (with-matcher m (type1 type2)
    (cond
      ((or (m integral integral)
           (m integral-vector integral-vector))
       (add-spirv `(spirv-core:i-add ,ret ,rtype ,a ,b)))
      ((or (m floating floating)
           (m floating-vector floating-vector))
       (add-spirv `(spirv-core:f-add ,ret ,rtype ,a ,b)))
      (t (error "can't add ~s to ~s" type1 type2)))))


(defint* - (a &optional b)
  (s->v-casts (integral integral-vector)
              (floating floating-vector))
  ;; int-int or ivec-ivec use i-sub
  ;; float-float or fvec-fvec use f-sub
  ;; int-NIL use s-negate
  ;; float-NIL use f-negate
  (with-matcher m (type1 type2)
    (cond
      ((or (m integral integral)
           (m integral-vector integral-vector))
       (add-spirv `(spirv-core:i-sub ,ret ,rtype ,a ,b)))
      ((or (m floating floating)
           (m floating-vector floating-vector))
       (add-spirv `(spirv-core:f-sub ,ret ,rtype ,a ,b)))
      ;; negate
      ((or (m integral nil)
           (m integral-vector nil))
       (add-spirv `(spirv-core:s-negate ,ret ,rtype ,a)))
      ((or (m floating nil)
           (m floating-vector nil))
       (add-spirv `(spirv-core:f-negate ,ret ,rtype ,a)))
      (t (error "can't subtract ~s from ~s" type1 type2)))))

(defint* 1+ (a)
  (with-matcher m (type1)
    (cond
      ((m integral)
       (add-spirv `(spirv-core:i-add ,ret ,rtype ,a (the ,type1 1))))
      ((m floating)
       (add-spirv `(spirv-core:f-add ,ret ,rtype ,a (the ,type1 1.0))))
      ((m integral-vector)
       (add-spirv
        `(spirv-core:i-add ,ret ,rtype ,a
                           (,type1 ,@ (loop repeat (scalar/vector-size .type1)
                                            collect 1)))))
      ((m floating-vector)
       (add-spirv
        `(spirv-core:f-add ,ret ,rtype ,a
                           (,type1 ,@ (loop repeat (scalar/vector-size .type1)
                                            collect 1.0)))))
      (t (error "can't add 1 to ~s" type1)))))

(defint* 1- (a)
  (with-matcher m (type1)
    (cond
      ((m integral)
       (add-spirv `(spirv-core:i-sub ,ret ,rtype ,a (the ,type1 1))))
      ((m floating)
       (add-spirv `(spirv-core:f-sub ,ret ,rtype ,a (the ,type1 1.0))))
      ((m integral-vector)
       (add-spirv
        `(spirv-core:i-sub ,ret ,rtype ,a
                           (,type1 ,@ (loop repeat (scalar/vector-size .type1)
                                            collect 1)))))
      ((m floating-vector)
       (add-spirv
        `(spirv-core:f-sub ,ret ,rtype ,a
                           (,type1 ,@ (loop repeat (scalar/vector-size .type1)
                                            collect 1.0)))))
      (t (error "can't subtract 1 from ~s" type1)))))


(defint* / (a b)
  (s->v-casts (integral integral-vector)
              (floating floating-vector))
  ;; if int-int or ivec-ivec use i-sub
  ;; if float-float or fvec-fvec use f-sub
  (with-matcher m (type1 type2)
    (cond
      ((or (m signed signed)
           (m signed-vector signed-vector))
       (add-spirv `(spirv-core:s-div ,ret ,rtype ,a ,b)))
      ((or (m unsigned unsigned)
           (m unsigned-vector unsigned-vector))
       (add-spirv `(spirv-core:u-div ,ret ,rtype ,a ,b)))
      ((or (m floating floating)
           (m floating-vector floating-vector))
       (add-spirv `(spirv-core:f-sub ,ret ,rtype ,a ,b)))
      (t (error "can't divide ~s by ~s" type1 type2)))))

(defint* 3bgl-glsl:mod (a b)
  (s->v-casts (integral integral-vector)
              (floating floating-vector))
  ;; if int-int or ivec-ivec use i-sub
  ;; if float-float or fvec-fvec use f-sub
  (with-matcher m (type1 type2)
    (cond
      ((or (m signed signed)
           (m signed-vector signed-vector))
       (add-spirv `(spirv-core:s-mod ,ret ,rtype ,a ,b)))
      ((or (m unsigned unsigned)
           (m unsigned-vector unsigned-vector))
       (add-spirv `(spirv-core:u-mod ,ret ,rtype ,a ,b)))
      ((or (m floating floating)
           (m floating-vector floating-vector))
       (add-spirv `(spirv-core:f-mod ,ret ,rtype ,a ,b)))
      (t (error "can't compile (mod ~s ~s)" type1 type2)))))


(defint 3bgl-glsl:modf (a b) ;; just alias to mod?
  (dump-internal-call '3bgl-glsl:mod (list a b)))

(defint* and (a b)
  ;; bool,bool -> bool
  (with-matcher m (type1 type2)
    (cond
      ((or (m :bool :bool)
           (m bool-vector bool-vector))
       (add-spirv `(spirv-core:logical-and ,ret ,rtype ,a ,b)))
      (t (error "can't compile (AND ~s ~s)" type1 type2)))))


(defint* or (a b)
  ;; bool,bool -> bool
  (with-matcher m (type1 type2)
    (cond
      ((or (m :bool :bool)
           (m bool-vector bool-vector))
       (add-spirv `(spirv-core:logical-or ,ret ,rtype ,a ,b)))
      (t (error "can't compile (OR ~s ~s)" type1 type2)))))

(defint* 3bgl-glsl:^^ (a b) ;; xor
  ;; bool,bool -> bool
  (with-matcher m (type1 type2)
    (cond
      ((or (m :bool :bool)
           (m bool-vector bool-vector))
       (add-spirv `(spirv-core:logical-not-equal ,ret ,rtype ,a ,b)))
      (t (error "can't compile (^^ ~s ~s)" type1 type2)))))

;; modf, matrix-comp-mult, dot, cross, outer-product
;; incf,decf,++,--
;; >,<,<=,>=,=,/
;; less-than, less-than-equal, greater-than, greater-than-equal
;; equal, not-equal
;; any, all
;; ^^,not
;; lognot,logior,logand,logxor
;; <<,>>,
;; min, max, clamp
;; sin,cos,tan,asin,acos,atan
;; sinh,cosh,tanh,asinh,acosh,atanh
;; pow,exp,log,exp2,log2,sqrt,inverse-sqrt
;; abs, signum, sign
;; floor, truncate, ceiling, trunc, round, round-even,  ceil, fract
;; mix, step, smooth-step
;; is-nan, is-inf
;; fma, frexp, ldexp
;; length, distance, normalize, transpose, determinant, inverse
;; ftransform?
;; face-forward, reflect, refract

;; int,uint,bool,float,double?,*vec*,*mat* constructors
;; uadd-carry, usub-borrow, umul-extended, imul-extended
;; float-bits-to-int, float-bits-to-uint, int-bits-to-float, uint-bits-to-float
;; pack-*, unpack-*
;; bitfield-extract, bitfield-insert, bitfield-reverse, bit-count
;; ldb, dpb
;; find-lsb, find-msb

;; emit-stream-vertex, end-stream-primitive, emit-vertex, end-primitive

;; texture*
;; image*
;; atomic*
;; dfdx*, dfdy*, fwidth*
;; interpolate-at-*
;; noise*?
;; *barrier*

;; aref, slot-value, @

;; return
;; :?,if

