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
               :dmat3x4 :dmat4x2 :dmat4x3))))

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

(defmacro with-matcher (name (type1 type2) &body body)
  (alexandria:once-only (type1 type2)
    `(macrolet ((,name (set1 set2)
                  (let ((set1 (expand-type-set set1))
                        (set2 (expand-type-set set2)))
                    `(progn
                       (format t "~&matching: ~s in ~s~%           ~s in ~s~%"
                               ,',type1 ',set1
                               ,',type2 ',set2)
                       (and (member ,',type1 ',set1)
                            (member ,',type2 ',set2))))))
       ,@body)))


(defint * (a b)
  (let* ((ret (spv-tmp))
         (types (gethash *current-call* *binding-types*))
         (rtype (name (first types)))
         (type1 (name (second types)))
         (type2 (name (third types))))
    (assert (and type1 type2))
    ;; most casts are handled by caller, but add special case for
    ;; int*ivec and ivec*int, since vector-times-scalar is float only
    (flet ((icast (t1 t2 v)
             (let* ((tmp (spv-tmp))
                    (size (scalar/vector-size t2))
                    (cast (name (aref (scalar/vector-set t1) size))))
               (add-spirv `(spirv-core:composite-construct
                            ,tmp ,cast ,@ (loop repeat size
                                                collect v)))
               (values tmp cast))))
      (with-matcher m (type1 type2)
        (cond
          ((m integral integral-vector)
           (setf (values a type1)
                 (icast (second types) (third types) a)))
          ((m integral-vector integral)
           (setf (values b type2)
                 (icast (third types) (second types) b))))))
    ;; normal rules: (check after expanding casts)
    ;;  if int*int or ivec*ivec use i-mul
    ;;  if float*float or fvec*fvec use f-mul
    ;;  if fvec * float or float * fvec use vector-times-scalar
    ;;  if mat * float or float * mat use matrix-times-scalar
    ;;  if mat * fvec use matrix-times-vector
    ;;  if fvec * mat use vector-times-matrix
    ;;  if mat * mat use matrix-times-matrix
    ;;  if ivec * int or int * ivec make temp ivec from int and use i-mul
    ;;
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
        (t (error "can't multiply ~s by ~s yet" type1 type2))))
    ret))
