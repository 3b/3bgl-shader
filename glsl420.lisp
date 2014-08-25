(in-package #:glsl)

;; define builtin types, probably should move builtin functions here
;; from glsl-base, and add separate package for each version...

(let ((3bgl-shaders::*environment* *glsl-base-environment*)
      (3bgl-shaders::*global-environment* *glsl-base-environment*)
      (walker (make-instance 'glsl-walker)))
  (macrolet ((input (name type stage &rest qualifiers)
               (declare (ignorable qualifiers))
               `(3bgl-shaders::walk '(input ,name ,type :stage ,stage
                                                       :internal t)
                                    walker))
             ;; alias for INPUT for now
             (const (name type stage &rest qualifiers)
               (declare (ignorable qualifiers))
               `(3bgl-shaders::walk '(input ,name ,type :stage ,stage
                                      :internal t)
                                    walker))
             (output (name type stage  &rest qualifiers)
               (declare (ignorable qualifiers))
               `(3bgl-shaders::walk '(output ,name ,type :stage ,stage
                                                        :internal t)
                                    walker))
             (interface (name (&rest bind) &body slots)
                 `(3bgl-shaders::walk '(interface ,name (,@bind :internal t)
                                        ,@slots)
                   walker)))

    ;; compute (430+)
    (input (gl-num-work-groups "gl_NumWorkGroups") :uvec3 :compute)
    (const (gl-work-group-size "gl_WorkGroupSize") :uvec3 :compute)
    (input (gl-work-group-id "gl_WorkGroupID") :uvec3 :compute)
    (input (gl-local-invocation-id "gl_LocalInvocationID") :uvec3 :compute)
    (input (gl-global-invocation-id "gl_GlobalInvocationID") :uvec3 :compute)
    (input (gl-local-invocation-index "gl_LocalInvocationIndex") :int :compute) 


    ;; vertex
    (input (gl-vertex-id "gl_VertexID") :int :vertex)
    (input (gl-instance-id "gl_InstanceID") :int :vertex)
    (interface (gl-per-vertex "gl_PerVertex")
        (:out (:vertex t
               :geometry t
               ;; fixme: should be gl_out[]
               :tess-control gl-out
               :tess-eval t)
              ;; fixme: should be gl_in[]
         :in (:geometry (gl-in "gl_in")
              :tess-control (gl-in "gl_in")
              :tess-eval (gl-in "gl_in")
              ))

      ((gl-position "gl_Position") :vec4)
      ((gl-point-size "gl_PointSize") :float)
      ((gl-clip-distance "gl_ClipDistance") (:float *)) ;; fixme: array syntax?
      ((gl-cull-distance "gl_CullDistance") (:float *)) ;; glsl 450
      )

    ;; geometry
    (input (gl-primitive-id-in "gl_PrimitiveIDIn") :int :geometry)
    (input (gl-invocation-id "gl_InvocationID") :int :geometry)
    (output (gl-primitive-id "gl_PrimitiveID") :int :geometry)
    (output (gl-layer "gl_Layer") :int :geometry) ;; 430?
    (output (gl-viewport-index "gl_ViewportIndex") :int :geometry) ;; 430?

    ;; tessellation control
    (input (gl-patch-vertices-in "gl_PatchVerticesIn") :int :tess-control)
    (input (gl-primitive-id "gl_PrimitiveID") :int :tess-control)
    (input (gl-invocation-id "gl_InvocationID") :int :tess-control)
    (output (gl-tess-level-outer "gl_TessLevelOuter") (:float 4) :tess-control :patch)
    (output (gl-tess-level-inner "gl_TessLevelInner") (:float 2) :tess-control :patch)

    ;; tessellation evaluation
    (input (gl-patch-vertices-in "gl_PatchVerticesIn") :int :tess-eval)
    (input (gl-primitive-id "gl_PrimitiveID") :int :tess-eval)
    (input (gl-tess-coord "gl_TessCoord") :vec3 :tess-eval)
    (input (gl-tess-level-outer "gl_TessLevelOuter" (:float 4)) :tess-eval :patch)
    (input (gl-tess-level-inner "gl_TessLevelInner" (:float 2)) :tess-eval :patch)

    ;; fragment
    (input (gl-frag-coord "gl_FragCoord") :vec4 :fragment)
    (input (gl-front-facing "gl_FrontFacing") :bool :fragment)
    (input (gl-clip-distance "gl_ClipDistance") (:float *) :fragment)
    (input (gl-cull-distance "gl_CullDistance") (:float *) :fragment) ;; 450
    (input (gl-point-coord "gl_PointCoord") :vec2 :fragment)
    (input (gl-primitive-ID "gl_PrimitiveID") :int :fragment)
    (input (gl-sample-id "gl_SampleID") :int :fragment)
    (input (gl-sample-position "gl_SamplePosition") :vec2 :fragment)
    (input (gl-sample-mask-in "gl_SampleMaskIn") (:int *) :fragment)
    (input (gl-layer "gl_Layer") :int :fragment) ;; 430?
    (input (gl-viewport-index "gl_ViewportIndex") :int :fragment) ;; 430?
    (input (gl-helper-invocation "gl_HelperInvocation") :bool :fragment) ;; 450
    (output (gl-frag-depth "gl_FragDepth") :float :fragment)
    (output (gl-sample-mask "gl_SampleMask") (:int *) :fragment)

))
