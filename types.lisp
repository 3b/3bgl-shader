(in-package #:3bgl-shaders)

(defclass generic-type ()
  ((name :accessor name :initarg :name)
   (glsl-name :accessor glsl-name :initarg :glsl-name :initform nil)
   ;; flag indicating we don't need to dump a definition into generated source
   (internal :accessor internal :initform nil :initarg :internal)   ))


(defclass concrete-type (generic-type)
  ())

(defclass aggregate-type (generic-type bindings)
  ())

(defclass struct-type (aggregate-type)
  ())

(defclass interface-type (aggregate-type)
  ())

#++ ;; uniform-blocks are a variant of interface...
(defclass uniform-block-type (aggregate-type)
  ())

(glsl::%glsl-macro defstruct (name args &body slots)
  (declare (ignore args))
  `(setf (gethash ',name (types *environment*))
         (make-instance 'struct-type
                        :name ',name
                        :bindings
                        ,(loop for (sname type . args) in slots
                              ;; should these be some 'slot-binding' type?
                              collect `(make-instance 'binding
                                                      :name ',sname
                                                      :value-type ',type)))))

(defclass interface-stage-binding (place)
  ((binding :accessor binding :initarg :binding)
   (stage :accessor stage :initarg :stage)
   (interface-qualifier :accessor interface-qualifier :initarg :interface-qualifier)
   (layout-qualifier :accessor layout-qualifier :initarg :layout-qualifier :initform nil)
   (interface-block :accessor interface-block :initform nil :initarg :interface-block)))

(defparameter *current-shader-stage* nil)

(defclass interface-binding (binding)
  ;; binding of a name to an interface in a particular stage
  ;; (just a plist stage -> binding for now, so we don't need to enumerate
  ;;  them all in advance...)
  ;; use stage T for default, or 'all stages' binding
  ((stage-bindings :accessor stage-bindings :initarg :stage-bindings
                   :initform nil)
   ;; flag indicating we don't need to dump a definition into generated source
   (internal :accessor internal :initform nil :initarg :internal)))

(defmethod stage-binding ((binding interface-binding))
  (let ((sb (or (getf (stage-bindings binding)
                      *current-shader-stage*)
                (getf (stage-bindings binding)
                      t))))
    sb))

(defmethod value-type ((binding interface-binding))
  (let ((sb (stage-binding binding)))
    (when sb
      (value-type sb))))

(defmethod value-type ((binding generic-type))
  binding)

(defmethod value-type ((binding interface-stage-binding))
  (value-type (binding binding)))



(defun bind-interface (stage type interface-qualifier name &key internal)
  (let ((type (or (get-type-binding type) type)))
    (cond
      ((eq name t)
       ;; binding an interface/uniform to T makes all of the slots visible
       ;; as bindings
       (loop for slot in (bindings type)
             for slot-name = (name slot)
             for slot-glsl-name = (glsl-name slot)
             for vb = (variable-bindings *environment*)
             do (unless (typep (gethash slot-name vb) 'interface-binding)
                  (setf (gethash slot-name vb)
                        (make-instance 'interface-binding
                                       :internal internal
                                       :name slot-name
                                       :glsl-name slot-glsl-name)))
                (setf (getf (stage-bindings (gethash slot-name vb)) stage)
                      (make-instance 'interface-stage-binding
                                     :stage stage
                                     :interface-qualifier interface-qualifier
                                     :binding slot
                                     :interface-block type))))
      (name
       ;; otherwise if we bind it with a name, make that name visible
       (let ((vb (variable-bindings *environment*)))
         (unless (typep (gethash name vb) 'interface-binding)
           (setf (gethash name vb) (make-instance 'interface-binding
                                                  :name name)))
         (setf (getf (stage-bindings (gethash name vb)) stage)
               (make-instance 'interface-stage-binding
                              :stage stage
                              :interface-qualifier interface-qualifier
                              :binding type)))))))

(glsl::%glsl-macro glsl::interface (%name (&key in out uniform internal)
                                         &body slots)
  ;; in/out/uniform are either T,NAME,or (&key :vertex :fragment ...)
  ;; where T means make slots directly visible in all stages,
  ;; NAME means make aggregate visible as NAME in all stages
  ;; and VERTEX/FRAGMENT/ETC means make aggregate visible with specified name(s)
  ;;   in specified stage(s)
  (let ((name (if (consp %name) (car %name) %name))
        (glsl-name (if (consp %name) (cadr %name))))
    (setf (gethash name (types *environment*))
          (make-instance 'interface-type
                         :name name
                         :glsl-name glsl-name
                         :internal internal
                         :bindings
                         (loop for (%sname type . args) in slots
                               for sname = (if (consp %sname)
                                              (first %sname)
                                              %sname)
                               for glsl-sname = (when (consp %sname)
                                                   (second %sname))
                               ;; should these be some 'slot-binding' type?
                               collect (make-instance 'binding
                                                      :name sname
                                                      :glsl-name glsl-sname
                                                      :value-type (get-type-binding type)))))
    (loop
      for (k x) on (list :in in :out out :uniform uniform) by #'cddr
      when (and x (eq x t))
        do (bind-interface t name k t :internal internal)
      else
        when x
          do (loop for (stage bind) on x by #'cddr
                   do (bind-interface stage name k bind :internal internal)))
    nil))

(defun in/out/uniform/attrib (qualifier %name type &key location internal stage)
  ;; possibly should have generic '&rest args' instead of enumerating options?
  (let ((vb (variable-bindings *environment*))
        (name (if (consp %name) (car %name) %name))
        (glsl-name (if (consp %name) (cadr %name))))
    (unless (typep (gethash name vb) 'interface-binding)
      (setf (gethash name vb) (make-instance 'interface-binding
                                             :internal internal
                                             :glsl-name glsl-name
                                             :name name)))
    (assert (equal (glsl-name (gethash name vb)) glsl-name))
    ;; just treating vs attributes and fs outputs like named interface
    ;; blocks for now
    (setf (getf (stage-bindings (gethash name vb)) stage)
          (make-instance 'interface-stage-binding
                         :stage stage
                         :interface-qualifier qualifier
                         :layout-qualifier (when location
                                             (list :location location ))
                         :binding (or (get-type-binding type) type))))
)

(glsl::%glsl-macro glsl::attribute (%name type &key location internal)
  (in/out/uniform/attrib :attribute %name type :location location :internal internal :stage :vertex)
  nil)

(glsl::%glsl-macro glsl::input (%name type &key location (stage :vertex)
                                      internal)
  (in/out/uniform/attrib :in %name type
                         :location location :internal internal :stage stage)
  nil)

(glsl::%glsl-macro glsl::output (%name type &key location (stage :fragment)
                                       internal)
  (in/out/uniform/attrib :out %name type
                         :location location :internal internal :stage stage)
  nil)

(glsl::%glsl-macro glsl::uniform (%name type &key location (stage t)
                                       internal)
  (in/out/uniform/attrib :uniform %name type
                         :location location :internal internal :stage stage)
  nil)


(glsl::%glsl-macro glsl::bind-interface (stage block-name interface-qualifier instance-name)
  (bind-interface stage block-name interface-qualifier instance-name)
  nil)

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
                  #++(format t "scope = ~s, ret = ~s~%"
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