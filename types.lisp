(in-package #:3bgl-shaders)

(defclass generic-type ()
  ((name :accessor name :initarg :name)))

(defclass concrete-type (generic-type)
  ((glsl-name :accessor glsl-name :initarg :glsl-name)))

(defclass set-type (generic-type)
  ((types :accessor types :initarg :types)))

(defparameter *known-declarations*
  '(declaration dynamic-extent ftype function ignore inline notinline
    optimize special type))

(defmethod process-type-declarations-for-scope (scope)
  (flet ((process-declaration (d a)
           (let ((type (get-type-binding d)))
             (unless type
               (warn "declared unknown type ~a for variables ~a?" d a))
             (loop for var in a
                   for b = (find var (bindings scope) :key 'name)
                   for bt = (value-type b)
                   do (when (not (member bt (list type d t)))
                        (warn "changing type of ~a from ~a to ~a?"
                              var (value-type b) type))
                      (setf (value-type b) type)))))
    (let ((declarations (declarations scope)))
      (loop for (decl . args) in (mapcan 'cdr declarations)
            do (cond
                 ;; handle explicit (type foo ...) declarations
                 ((eql decl 'type)
                  (process-declaration (car args) (cdr args)))
                 ;; handle 'values' for return type
                 ((eql decl 'values)
                  (format t "scope = ~s, ret = ~s~%"
                          scope
                          args)
                  (setf (return-type scope)
                        (if args
                            (get-type-binding (car args))
                            (get-type-binding :void)))
                  )
                 ;; ignore any other known declarations for now
                 ((member decl *known-declarations*)
                  )
                 ;; otherwise try to use it as a type declaration
                 (t
                  (process-declaration decl args))))))
  ;; return the scope, so we can just wrap ceration of an instance
  ;; with this to process the declaration
  scope)


(defun add-concrete-type (name glsl-name &key (env *environment*) type)
  (setf (gethash name (types env))
        (or type
            (make-instance 'concrete-type
                           :name name
                           :glsl-name glsl-name))))

(let ((*environment* glsl::*glsl-base-environment*))
  (add-concrete-type :void "void")
  (add-concrete-type :bool "bool")
  (add-concrete-type :int "int")
  (add-concrete-type :uint "uint")
  (add-concrete-type :float "float")
  (add-concrete-type :double "double")
  (add-concrete-type :vec2 "vec2")
  (add-concrete-type :vec3 "vec3")
  (add-concrete-type :vec4 "vec4")
  (add-concrete-type :dvec2 "dvec2")
  (add-concrete-type :dvec3 "dvec3")
  (add-concrete-type :dvec4 "dvec4")
  (add-concrete-type :bvec2 "bvec2")
  (add-concrete-type :bvec3 "bvec3")
  (add-concrete-type :bvec4 "bvec4")
  (add-concrete-type :ivec2 "ivec2")
  (add-concrete-type :ivec3 "ivec3")
  (add-concrete-type :ivec4 "ivec4")
  (add-concrete-type :uvec2 "uvec2")
  (add-concrete-type :uvec3 "uvec3")
  (add-concrete-type :uvec4 "uvec4")
  (add-concrete-type :mat2 "mat2")
  (add-concrete-type :mat3 "mat3")
  (add-concrete-type :mat4 "mat4")
  (add-concrete-type :mat2x2 "mat2x2")
  (add-concrete-type :mat2x3 "mat2x3")
  (add-concrete-type :mat2x4 "mat2x4")
  (add-concrete-type :mat3x2 "mat3x2")
  (add-concrete-type :mat3x3 "mat3x3")
  (add-concrete-type :mat3x4 "mat3x4")
  (add-concrete-type :mat4x2 "mat4x2")
  (add-concrete-type :mat4x3 "mat4x3")
  (add-concrete-type :mat4x4 "mat4x4")
  (add-concrete-type :dmat2 "dmat2")
  (add-concrete-type :dmat3 "dmat3")
  (add-concrete-type :dmat4 "dmat4")
  (add-concrete-type :dmat2x2 "dmat2x2")
  (add-concrete-type :dmat2x3 "dmat2x3")
  (add-concrete-type :dmat2x4 "dmat2x4")
  (add-concrete-type :dmat3x2 "dmat3x2")
  (add-concrete-type :dmat3x3 "dmat3x3")
  (add-concrete-type :dmat3x4 "dmat3x4")
  (add-concrete-type :dmat4x2 "dmat4x2")
  (add-concrete-type :dmat4x3 "dmat4x3")
  (add-concrete-type :dmat4x4 "dmat4x4")
  (add-concrete-type :sampler-1d "sampler1D")
  (add-concrete-type :sampler-2d "sampler2D")
  (add-concrete-type :sampler-3d "sampler3D")
  (add-concrete-type :sampler-cube "samplerCube")
  (add-concrete-type :sampler-2d-rect "sampler2DRect")
  (add-concrete-type :sampler-1d-shadow "sampler1DShadow")
  (add-concrete-type :sampler-2d-shadow "sampler2DShadow")
  (add-concrete-type :sampler-2d-rect-shadow "sampler2DRectShadow")
  (add-concrete-type :sampler-1d-array "sampler1DArray")
  (add-concrete-type :sampler-2d-array "sampler2DArray")
  (add-concrete-type :sampler-1d-array-shadow "sampler1DArrayShadow")
  (add-concrete-type :sampler-2d-array-shadow "sampler2DArrayShadow")
  (add-concrete-type :sampler-buffer "samplerBuffer")
  (add-concrete-type :sampler-2d-ms "sampler2DMS")
  (add-concrete-type :sampler-2d-ms-array "sampler2DMSArray")
  (add-concrete-type :sampler-cube-shadow "samplerCubeShadow")
  (add-concrete-type :sampler-cube-array "samplerCubeArray")
  (add-concrete-type :sampler-cube-array-shadow "samplerCubeArrayShadow")
  (add-concrete-type :isampler-1d "isampler1D")
  (add-concrete-type :isampler-2d "isampler2D")
  (add-concrete-type :isampler-3d "isampler3D")
  (add-concrete-type :isampler-cube "isamplerCube")
  (add-concrete-type :isampler-2d-rect "isampler2DRect")
  (add-concrete-type :isampler-1d-array "isampler1DArray")
  (add-concrete-type :isampler-2d-array "isampler2DArray")
  (add-concrete-type :isampler-buffer "isamplerBuffer")
  (add-concrete-type :isampler-2d-ms "isampler2DMS")
  (add-concrete-type :isampler-2d-ms-array "isampler2DMSArray")
  (add-concrete-type :isampler-cube-array "isamplerCubeArray")
  (add-concrete-type :usampler-1d "usampler1D")
  (add-concrete-type :usampler-2d "usampler2D")
  (add-concrete-type :usampler-3d "usampler3D")
  (add-concrete-type :usampler-cube "usamplerCube")
  (add-concrete-type :usampler-2d-rect "usampler2DRect")
  (add-concrete-type :usampler-1d-array "usampler1DArray")
  (add-concrete-type :usampler-2d-array "usampler2DArray")
  (add-concrete-type :usampler-buffer "usamplerBuffer")
  (add-concrete-type :usampler-2d-ms "usampler2DMS")
  (add-concrete-type :usampler-2d-ms-array "usampler2DMSArray")
  (add-concrete-type :usampler-cube-array "usamplerCubeArray"))




(defun add-set-type (name types &key (env *environment*) type)
  (setf (gethash name (types env))
        (or type
            (make-instance 'set-type
                           :name name
                           :types types))))

(let ((*environment* glsl::*glsl-base-environment*))
  (add-set-type :bvec '(:bvec2 :bvec3 :bvec4))
  (add-set-type :ivec '(:ivec2 :ivec3 :ivec4))
  (add-set-type :uvec '(:uvec2 :uvec3 :uvec4))
  (add-set-type :vec '(:vec2 :vec3 :vec4))
  (add-set-type :dvec '(:dvec2 :dvec3 :dvec4))
  ;;
  (add-set-type :gen-type '(:float :vec2 :vec3 :vec4))
  (add-set-type :gen-b-type '(:bool :bvec2 :bvec3 :bvec4))
  (add-set-type :gen-i-type '(:int :ivec2 :ivec3 :ivec4))
  (add-set-type :gen-u-type '(:uint :uvec2 :uvec3 :uvec4))
  (add-set-type :gen-d-type '(:double :dvec2 :dvec3 :dvec4))

  (add-set-type :mat '(:mat2   :mat2x3 :mat2x4
                       :mat3x2 :mat3   :mat3x4
                       :mat4x2 :mat4x3 :mat4)))

#++
(add-implicit-conversions
 (:int :uint :float :double)
 (:uint :float :double)
 (:float :double)
 (:ivec2 :uvec2 :vec2 :dvec2)
 (:ivec3 :uvec3 :vec3 :dvec3)
 (:ivec4 :uvec4 :vec4 :dvec4)
 (:uvec2 :vec2 :dvec2)
 (:uvec3 :vec3 :dvec3)
 (:uvec4 :vec4 :dvec4)
 (:vec2 :dvec2)
 (:vec3 :dvec3)
 (:vec4 :dvec4)
 (:mat2 :dmat2)
 (:mat3 :dmat3)
 (:mat4 :dmat4)
 (:mat2x3 :dmat2x3)
 (:mat2x4 :dmat2x4)
 (:mat3x2 :dmat3x2)
 (:mat3x4 :dmat3x4)
 (:mat4x2 :dmat4x2)
 (:mat4x3 :dmat4x3)
)