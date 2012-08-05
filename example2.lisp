;; skinning shader


#++(def-attributes ()
  ((position :vec4)
   (normal :vec3)
   (tangent :vec3)
   (bitangent :vec3)
   (uv :vec2)
   (bone-weights :vec4)
   (bone-indices :ivec4)))
;; probably shouldn't specify locations in general, just declare names/types
;; and specific locations can be added at compilation time if desired
;; (which would allow tree-shakers to drop unused inputs, and avoid
;;  having to count sizes by hand for arrays/dvecs/matrices/etc)
(attribute position :vec4 :location 0)
(attribute normal :vec3)
(attribute tangent :vec3)
(attribute bitangent :vec3)
(attribute uv :vec2)
(attribute bone-weights :vec4)
(attribute bone-indices :ivec4)

(interface transforms (:uniform t)
  (mvp :mat4)
  (normal-matrix :mat4))

(interface bones (:uniform (:vertex t))
  (bone-matrices (:mat4 32)))

(interface interpolated-vars ()
  (position :vec4)
  (normal :vec3)
  (tangent :vec3)
  (bitangent :vec3)
  (color :vec4)
  (uv :vec3))


(bind-interface :vertex interpolated-vars :out out)

(bind-interface :fragment interpolated-vars :in t)


(defshader skin :vertex ()
  (macrolet ((weight (x)
               `(* (aref bone-matrices (@ bone-indices ,x))
                   vertex
                   (@ bone-weights ,x))))
    (let ((pos (+ (weight x)
                  (weight y)
                  (weight z)
                  (weight w))))
      (setf (@ out position) (* mvp pos)
            (@ out normal) (* normal-matrix normal)
            (@ out tangent) (* normal-matrix tangent)
            (@ out bitangent) (* normal-matrix bitangent)
            (@ out uv) uv
            (@ out color) (vec4 1.0 1.0 1.0 1.0)))))



(interface material ()
  (emission :vec4)
  (ambient :vec4)
  (diffuse :vec4)
  (specular :vec4)
  (gloss :float)
  (diffuse-texture :sampler-2d)
  (specular-texture :sampler-2d)
  (normal-texture :sampler-2d))


(bind-interface :fragment material :uniform material)

(interface light (:in (:fragment light))
  (color :vec4)
  (position :vec4))



;;; not sure if we should merge vs inputs and fs outputs with def-interface
;;; and just expand it to something else, or have separate macros?
#++(def-outputs ()
  ((color :vec4))
  out)
;; -> layout(location = 0) out vec4 color
(output color :vec4 :location 0)

;; (output color :vec4 :location 3 :index 1 :count 3)
;; -> layout(location = 3, index = 1) out vec4 colors[3]

;; not sure if outputs should need to specify locations/indices by hand or not?
;; if possible probably best to let wrapper/compiler add specific indices
;; since that makes it easier to target older versions


(defun light (view-direction normal
              diffuse-color specular-color gloss
              light-direction light-distance light-color)
  ;; see http://www.altdevblogaday.com/2011/08/23/shader-code-for-physically-based-lighting/
  ;; view-direction, normal and light-direction should be normalized vectors
  ;; in same coordinate space (probably tangent space for normal mapping)
  (let* ((half-vector (normalize (+ light-direction view-direction)))
         (n-dot-v (max 0 (dot normal view-direction)))
         (n-dot-l (max 0 (dot normal light-direction)))
         (n-dot-h (max 0 (dot normal half-vector)))
         (h-dot-l (max 0 (dot half-vector light-direction)))
         (diffuse (* diffuse-color light-color n-dot-l))
         (blinn-phong (expt n-dot-h gloss))
         ;; including pi/4 from brdf in normalization term
         (normalization (/ (+ gloss 2) 8))
         (cosine n-dot-l)
         (fresnel (+ specular-color
                     (* (- 1 specular-color) (expt (- 1 h-dot-l) 5))))
         ;; pi should probably be a single-float constant, not CL:PI
         (alpha (/ (sqrt (+ (/ pi 2) (* (/ pi 4) gloss)))))
         (visibility (/ (* (+ alpha (* n_dot_l (- 1 alpha)))
                           (+ alpha (* n_dot_v (- 1 alpha))))))
         (specular (* blinn-phong normalization cosine fresnel visibility))))
  (+ diffuse specular))

;; probably want to name these something other than `main`, and rename
;; during export, so we can have multiple possible shaders defined at once
;; and just pick the specific one as needed for a given shader
(defun main ()
  (let* ((ntex (texture-2d normal-texture uv))
         (dtex (texture-2d diffuse-texture uv))
         (stex (texture-2d specular-texture uv))
         ;; probably should renormalize/othogonalize tangent space
         ;; if doing this in frag shader.
         (tangent-space (mat3 normal tangent bitangent))
         ;; probably should calculate light/eye dir in vertex shader
         ;; and interpolate instead of calculating per fragment?
         (light-dir (- (.position light) position)))
    (setf (.color out) (light (normalize (* tangent-space (- position))) ntex
                              dtex (vec3 stex) (.a stex)
                              (normalize (* tangent-space light-dir))
                              (length light-dir)
                              (.color light)))))