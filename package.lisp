(defpackage #:3bgl-shaders
  (:use :cl)
  (:intern #:%glsl-macro
           #:*package-environments*
           #:ensure-package-environment)
  (:export
   :layout
   :in
   :out
   :inout
   :generate-stage
   :compile-form
   :stage
   :*modified-function-hook*
   :*default-version*))

(defpackage #:3bgl-glsl
  (:use :cl)
  (:shadow #:defun
           #:defconstant
           #:defmacro
           #:defstruct)
  (:import-from #:3bgl-shaders
                #:layout
                #:in
                #:out
                #:inout
                #:stage
                #:%glsl-macro
                #:*package-environments*
                #:ensure-package-environment)
  (:export
   ;; shadowed from CL
   #:defun
   #:defmacro
   #:defconstant
   #:defstruct

   ;; ??
   #:generate-stage
   #:glsl-defun
   #:glsl-interface
   #:glsl-attribute
   #:glsl-output
   #:glsl-input
   #:glsl-uniform
   #:glsl-defconstant

   ;; glsl-side API
   #:layout
   #:in
   #:out
   #:inout
   #:interface
   #:attribute
   #:output
   #:@
   #:input
   #:uniform
   #:shared
   #:bind-interface
   #:stage

   ;; predefined constants
   #:gl-vertex-id
   #:gl-instance-id
   #:gl-draw-id
   #:gl-base-vertex
   #:gl-base-instance
   #:gl-per-vertex
   #:gl_out ;;?
   #:gl_in ;;?
   #:gl-in
   #:gl-position
   #:gl-point-size
   #:gl-clip-distance
   #:gl-primitive-id-in
   #:gl-invocation-id
   #:gl-primitive-id
   #:gl-layer
   #:gl-viewport-index
   #:gl-patch-vertices-in
   #:gl-tess-level-outer
   #:gl-tess-level-inner
   #:gl-tess-coord
   #:gl-frag-coord
   #:gl-front-facing
   #:gl-point-coord
   #:gl-sample-id
   #:gl-sample-position
   #:gl-sample-mask-in
   #:gl-frag-depth
   #:gl-sample-mask
   #:gl-num-work-groups
   #:gl-work-group-size
   #:gl-work-group-id
   #:gl-local-invocation-id
   #:gl-global-invocation-id
   #:gl-local-invocation-index

   ;; built-in glsl functions
   ;; many of these are just reexported from CL#: though meaning might
   ;; differe a bit

   #:<<
   #:>>
   #:^^
   #:radians
   #:degrees
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:sinh
   #:cosh
   #:tanh
   #:asinh
   #:acosh
   #:atanh
   #:pow
   #:exp
   #:log
   #:exp2
   #:log2
   #:sqrt
   #:inverse-sqrt
   #:abs
   #:signum
   #:sign
   #:floor
   #:truncate
   #:trunc
   #:round
   #:round-even
   #:ceiling
   #:ceil
   #:fract
   #:mod
   #:modf
   #:min
   #:max
   #:clamp
   #:mix
   #:step
   #:smooth-step
   #:is-nan
   #:is-inf
   #:float-bits-to-int
   #:float-bits-to-uint
   #:int-bits-to-float
   #:uint-bits-to-float
   #:fma
   #:frexp
   #:ldexp
   ;; 8.4 floating-point pack and unpack functions
   #:pack-unorm-2x16
   #:pack-snorm-2x16
   #:pack-unorm-4x8
   #:pack-snorm-4x8
   #:unpack-unorm-2x16
   #:unpack-snorm-2x16
   #:unpack-unorm-4x8
   #:unpack-snorm-4x8
   #:pack-double-2x32
   #:unpack-double-2x32
   #:pack-half-2x16
   #:unpack-half-2x16
   ;; 8.5 geometric functions
   #:length
   #:distance
   #:dot
   #:cross
   #:normalize
   ;; compat/vertex shader only
   #:ftransform
   #:face-forward
   #:reflect
   #:refract
   ;; 8.6 matrix functions
   #:matrix-comp-mult
   #:outer-product
   #:transpose
   #:determinant
   #:inverse
   ;; 8.7 vector relational functions
   #:less-than
   #:less-than-equal
   #:greater-than
   #:greater-than-equal
   #:equal
   #:not-equal
   #:any
   #:all
   ;; 8.8 integer functions
   #:uadd-carry
   #:usub-borrow
   #:umul-extended
   #:imul-extended
   #:bitfield-extract
   #:bitfield-insert
   #:bitfield-reverse
   #:bit-count
   #:find-lsb
   #:find-msb
   ;; 8.9 Texture Functions
   #:texture-size
   #:texture-query-lod
   #:texture-query-levels
   #:texture-samples
   #:texture
   #:texture-proj
   #:texture-lod
   #:texture-offset
   #:texel-fetch
   #:texel-fetch-offset
   #:texture-proj-offset
   #:texture-lod-offset
   #:texture-proj-lod
   #:texture-proj-lod-offset
   #:texture-grad
   #:texture-grad-offset
   #:texture-proj-grad
   #:texture-proj-grad-offset
   ;; 8.9.3 texture gather functions
   #:texture-gather
   #:texture-gather-offset
   #:texture-gather-offsets
   ;; 8.9.4 compatibility profile
   #:texture-1d
   #:texture-1d-proj
   #:texture-1d-lod
   #:texture-1d-proj-lod
   #:texture-2d
   #:texture-2d-proj
   #:texture-2d-lod
   #:texture-2d-proj-lod
   #:texture-3d
   #:texture-3d-proj
   #:texture-3d-lod
   #:texture-3d-proj-lod
   #:texture-cube
   #:texture-cube-lod
   #:shadow-1d
   #:shadow-2d
   #:shadow-1d-proj
   #:shadow-2d-proj
   #:shadow-1d-lod
   #:shadow-2d-lod
   #:shadow-1d-proj-lod
   #:shadow-2d-proj-lod
   ;; 8.10 atomic-counter functions
   #:atomic-counter-increment
   #:atomic-counter-decrement
   #:atomic-counter
   ;; 8.11 atomic memory functions
   #:atomic-add
   #:atomic-min
   #:atomic-max
   #:atomic-and
   #:atomic-or
   #:atomic-xor
   #:atomic-exchange
   #:atomic-comp-swap
   ;; 8.12 Image functions
   #:image-size
   #:image-samples
   #:image-load
   #:image-store
   #:image-atomic-add
   #:image-atomic-min
   #:image-atomic-max
   #:image-atomic-and
   #:image-atomic-or
   #:image-atomic-xor
   #:image-atomic-exchange
   #:image-atomic-comp-swap
   ;; 8.13 fragment processing functions
   ;; 8.13.1 derivative functions
   #:dfdx
   #:dfdy
   #:dfdx-fine
   #:dfdy-fine
   #:dfdx-coarse
   #:dfdy-coarse
   #:fwidth
   #:fwidth-fine
   #:fwidth-coarse
   ;; 8.13.2 interpolation functions
   ;; these specify float/vec2/vec3/vec4 explicitly instead of gentype?
   #:interpolate-at-centroid
   #:interpolate-at-sample
   #:interpolate-at-centroid
   ;; 8.14 noise functions
   #:noise1
   #:noise2
   #:noise3
   #:noise4
   ;; 8.15 geometry shader functions
   #:emit-stream-vertex
   #:end-stream-primitive
   #:emit-vertex
   #:end-primitive
   ;; 8.16 shader invocation control functions
   #:barrier
   ;; 8.17 Shader memory control functions
   #:memory-barrier
   #:memory-barrier-atomic-counter
   #:memory-barrier-buffer
   #:memory-barrier-shared
   #:memory-barrier-image
   #:group-memory-barrier
   ;; 8.19 Shader Invocation Group Functions
   #:any-invocation
   #:all-invocations
   #:all-invocations-equal

   ;; vector/matrix constructors
   #:int8
   #:int16
   #:int
   #:int64
   #:uint8
   #:uint16
   #:uint
   #:uint64
   #:bool
   #:float
   #:double
   #:bvec2
   #:bvec3
   #:bvec4
   #:i8vec2
   #:i8vec3
   #:i8vec4
   #:u8vec2
   #:u8vec3
   #:u8vec4
   #:i16vec2
   #:i16vec3
   #:i16vec4
   #:u16vec2
   #:u16vec3
   #:u16vec4
   #:ivec2
   #:ivec3
   #:ivec4
   #:uvec2
   #:uvec3
   #:uvec4
   #:i64vec2
   #:i64vec3
   #:i64vec4
   #:u64vec2
   #:u64vec3
   #:u64vec4
   #:f16vec2
   #:f16vec3
   #:f16vec4
   #:vec2
   #:vec3
   #:vec4
   #:dvec2
   #:dvec3
   #:dvec4
   #:f16mat2
   #:f16mat2x3
   #:f16mat2x4
   #:f16mat3x2
   #:f16mat3
   #:f16mat3x4
   #:f16mat4x2
   #:f16mat4x3
   #:f16mat4
   #:mat2
   #:mat2x3
   #:mat2x4
   #:mat3x2
   #:mat3
   #:mat3x4
   #:mat4x2
   #:mat4x3
   #:mat4
   #:dmat2
   #:dmat2x3
   #:dmat2x4
   #:dmat3x2
   #:dmat3
   #:dmat3x4
   #:dmat4x2
   #:dmat4x3
   #:dmat4
   ;; misc
   #:discard
   ))

;;; package intended to be :USEd by shader code in place of :cl
;;; exports all symbols from 3bgl-glsl, and any CL symbols not
;;; shadowed by it
;;; FIXME: probably should rename this one 3BGL-GLSL, and give the internal package the longer name?
(defpackage #:3bgl-glsl/cl
  (:use #:cl #:3bgl-glsl)
  (:shadowing-import-from #:3bgl-glsl #:defun #:defconstant
                          #:defmacro #:defstruct)
  #. (cons :export
           (flet ((externals (x)
                    (let ((a))
                      (do-external-symbols (s x)
                        (push s a))
                      a)))
             (append (externals '#:3bgl-glsl)
                     (loop for s in (externals '#:cl)
                           unless
                           (eq :external
                               (nth-value 1 (find-symbol (symbol-name s)
                                                         '#:3bgl-glsl)))
                           collect s)))))

