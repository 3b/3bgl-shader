(defpackage #:3bgl-shader-example-shaders
  (:use #:3bgl-glsl/cl)
  (:export
   #:minimal-vertex
   #:minimal-fragment)
  ;; 3bgl-shader doesn't let you define globals named by symbols from
  ;; CL or 3bgl packages, so shadow TIME locally
  (:shadow #:time))
(in-package #:3bgl-shader-example-shaders)


;; vertex shader inputs, we specify :location so we don't have to
;; query the attributes from host code and can just use the specified
;; locations (CL:POSITION is predefined as :vec4 :location 0 since it
;; is pretty common to want that, so we can re-specify that exact
;; definition if we want to, but can't change it)
(input position :vec4 :location 0)

;; if we were using modern GL, we would define some more vertex attributes
;; (input normal :vec3 :location 1)

;; for glutSolidTeapot we need to use the compatibility mode
;; attributes, so we specify the GLSL name of the fixed-function
;; vertex attributes instead of a location
(input (normal "gl_Normal") :vec3)
(input (uv "gl_MultiTexCoord0") :vec4)

;; fragment shader output
(output color :vec4 :stage :fragment)

;; uniforms
(uniform mv :mat4) ;; model-view matrix
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform normal-matrix :mat4)
(uniform time :float)


;; output from vertex shader, interpolated by GL then sent as input to
;; fragment shader
;; visible in vertex shader as 'outs', and in fragment shader as 'ins'
(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec4)
  (normal :vec3)
  (color :vec4)
  (uv :vec4)
  (eye-direction :vec3)
  (light-direction :vec3))

;; minimal shader program, just draw the object in solid red
(defun minimal-vertex ()
  ;; transform the input vertex position by the current
  ;; modelview-projection matrix and assign it to the built-in
  ;; GL-POSITION variable
  (setf gl-position (* mvp position)))

(defun minimal-fragment ()
  ;; set the output color to opaque red (R G B A =  1 0 0 1)
  (setf color (vec4 1 0 0 1)))



;; bit more complex, interpolate normals, and draw those to color
(defun normal-vertex ()
  (setf gl-position (* mvp position))
  ;; transform normals and send to fragment shader
  (setf (@ outs normal) (* (mat3 normal-matrix) normal)))

(defun normal-fragment ()
  ;; just set the color to the interpolated normal, with alpha set to 1
  (setf color (vec4 (@ ins normal) 1)))

;; some constants for lighting (would probably be uniforms in a real program)
(defconstant +ambient-color+ (vec3 0.0 0.024 0.06) :vec3)
(defconstant +diffuse-color+ (vec3 0.6 0.4 0.2)#++(vec3 0.2 0.4 0.6) :vec3)
(defconstant +specular-exponent+ 16 :float)
(defconstant +light-position+ (vec3 4 4 -5) :vec3)

;; generic vertex shader used for a few lighting models
(defun lighting-vertex ()
  (setf gl-position (* mvp position))
  (setf (@ outs normal) (* (mat3 normal-matrix) normal)
        (@ outs position) (* mv position)
        ;; interpolated lighting parameters
        (@ outs light-direction) (- +light-position+ (vec3 (* mv position)))
        (@ outs eye-direction) (- (vec3 (* mv position)))))

(defun simple-lighting-fragment ()
  ;; normalize the interpolated normal, since interpolation doesn't
  ;; preserve length
  (let* ((normal (normalize (@ ins normal)))
         ;; same for eye direction and light direction
         (eye-direction (normalize (@ ins eye-direction)))
         (light-direction (normalize (@ ins light-direction)))
         ;; calculate some intermediate values
         (l-dot-n (clamp (dot light-direction normal) 0 1))
         (r (reflect (- light-direction) normal))
         (r-dot-v (clamp (dot r eye-direction) 0 1))
         (distance (length (@ ins eye-direction))))
    (setf color (vec4 (+ +ambient-color+
                         (* (/ 2 distance)
                            (+ (* +diffuse-color+ l-dot-n)
                               (pow r-dot-v +specular-exponent+))))
                      1))))


;; add some random hacks to the lighting to make it look shinier
;; and factor out the lighting calculation so it can be reused
(defun shiny-lighting (normal eye-dir light-dir distance)
  (let* ((l-dot-n (clamp (dot light-dir normal) 0 1))
         (r (reflect (- light-dir) normal))
         (r-dot-v (clamp (dot r eye-dir) 0 1))
         ;; 0 when normal is perpendicular to view, used to fake
         ;; lambertian reflection
         (edge (dot eye-dir normal))
         ;; factor out the specular calculation, and color the light a bit
         (spec (* (vec3 1 0.4 0.4)
                  (pow r-dot-v +specular-exponent+))))
    ;; make a sharp drop in specular highlight so it looks like
    ;; shinier reflection of a larger light instead of point
    (if (> (length spec) 0.8)
        (setf spec (* 0.8 (normalize spec)))
        (setf spec (* spec spec)))
    (return (vec4 (+ +ambient-color+
                     (* (/ 2 distance)
                        (+ (* +diffuse-color+
                              l-dot-n)
                           spec))
                     ;; fake lambert term
                     ;; (everything is reflective as you approach 0 angle)
                     (if (< edge 0.4)
                         (* (clamp (- 1 (expt (/ edge 0.4) 2)) 0.0 1.0)
                            (vec3 0.0 0.15 0.25))
                         (vec3 0)))
                  1))))

(defun shiny-fragment ()
  (let* ((normal (normalize (@ ins normal)))
         (eye-direction (normalize (@ ins eye-direction)))
         (light-direction (normalize (@ ins light-direction)))
         (distance (length (@ ins eye-direction))))
    (setf color
          (shiny-lighting normal eye-direction light-direction distance))))

;; 4d perlin noise from "Efficient computational noise in GLSL"
;; https://github.com/ashima/webgl-noise/blob/master/src/noise4D.glsl
;; (original is MIT license)

(defun mod289 (x)
  (return (- x (* 289 (floor (* x (/ 289)))))))

(defun permute (x)
  (return (mod289 (* x (+ 1 (* x 34))))))

(defun grad4 (j ip)
  (let ((ones (ivec4 1 1 1 -1))
        (p)
        (s))
    (setf (.xyz p) (- (* (floor (* (fract (* (vec3 j) (.xyz ip))) 7))
                         (.z ip))
                      1)
          (.w p) (- 1.5 (dot (abs (.xyz p)) (.xyz ones)))
          s (vec4 (less-than p (vec4 0)))
          (.xyz p) (+ (.xyz p) (* (1- (* (.xyz s) 2))
                                  (.www s))))
    (return p)))

(defconstant +f4+ 0.309016994374947451 :float)

(defun snoise (v)
  (let* ((c (vec4 0.138196601125011 ; (5 - sqrt(5))/20 G4
                  0.276393202250021 ; 2 * G4
                  0.414589803375032 ; 3 * G4
                  -0.447213595499958)) ; -1 + 4 * G4
         ;; first corner
         (i (floor (+ v (dot v (vec4 +f4+)))))
         (x0 (+ (- v i) (dot i (.xxxx c))))
         ;; other corners
         (i0)
         (is-x (step (.yzw x0) (.xxx x0)))
         (is-yz (step (.zww x0) (.yyz x0))))
    (setf (.x i0) (+ (.x is-x) (.y is-x) (.z is-x))
          (.yzw i0) (- 1 is-x))
    (incf (.y i0) (+ (.x is-yz) (.y is-yz)))
    (incf (.zw i0) (- 1 (.xy is-yz)))
    (incf (.z i0) (.z is-yz))
    (incf (.w i0) (- 1.0 (.z is-yz)))
    ;; i0 now contains the unique values 0,1,2,3 in each channel
    (let* ((i3 (clamp i0 0.0 1.0))
           (i2 (clamp (- i0 1.0) 0.0 1.0))
           (i1 (clamp (- i0 2.0) 0.0 1.0))
           (x1 (+ (- x0 i1) (.xxxx C)))
           (x2 (+ (- x0 i2) (.yyyy C)))
           (x3 (+ (- x0 i3) (.zzzz C)))
           (x4 (+ x0 (.wwww C)))
           (i (mod289 i))
           (j0 (permute (+ (permute (+ (permute (+ (permute (.w i))
                                                   (.z i)))
                                       (.y i)))
                           (.x i))))
           (j1 (macrolet ((p-i* (p z)
                            `(permute (+ ,@(when p (list p))
                                         (,z i)
                                         (vec4 (,z i1) (,z i2)
                                               (,z i3) 1.0)))))
                 (p-i* (p-i* (p-i* (p-i* nil .w) .z) .y) .x)))
           (ip (vec4 (/ 294.0) (/ 49.0) (/ 7.0) 0.0))
           (p0 (grad4 j0 ip))
           (p1 (grad4 (.x j1) ip))
           (p2 (grad4 (.y j1) ip))
           (p3 (grad4 (.z j1) ip))
           (p4 (grad4 (.w j1) ip))
           (norm (inverse-sqrt (vec4 (dot p0 p0)
                                     (dot p1 p1)
                                     (dot p2 p2)
                                     (dot p3 p3)))))
      (setf p0 (* p0 (.x norm))
            p1 (* p1 (.y norm))
            p2 (* p2 (.z norm))
            p3 (* p3 (.w norm))
            p4 (* p4 (inverse-sqrt (dot p4 p4))))
      (let* ((m0 (max (- 0.6 (vec3 (dot x0 x0) (dot x1 x1) (dot x2 x2)))
                      0.0))
             (m1 (max (- 0.6 (vec2 (dot x3 x3) (dot x4 x4)))
                      0.0)))
        (setf m0 (* m0 m0)
              m1 (* m1 m1))
        (return (* 49 (+ (dot (* m0 m0) (vec3 (dot p0 x0)
                                              (dot p1 x1)
                                              (dot p2 x2)))
                         (dot (* m1 m1) (vec2 (dot p3 x3)
                                              (dot p4 x4))))))))))

;; now make a fragment shader that uses perlin noise to distort the normals
(defun noisy-shiny-fragment ()
  (let* ((normal (@ ins normal))
         (eye-direction (normalize (@ ins eye-direction)))
         (light-direction (normalize (@ ins light-direction)))
         (distance (length (@ ins eye-direction)))
         ;; calculate 3 octaves of noise based on fragment world
         ;; position and time
         (noise (+ (snoise (vec4 (* 2 (vec3 (@ ins position)))
                                 (/ time 5)))
                   (* 0.5
                      (snoise (vec4 (* 5.1 (vec3 (@ ins position)))
                                    (/ time 4))))
                   (* 0.2
                      (snoise (vec4 (* 8.1 (vec3 (@ ins position)))
                                    (/ time 2))))))
         ;; get derivatives of noise function
         ;; (probably should calculate directly and/or tweak glsl
         ;;  sampling/interpolation settings, but works well enough
         ;;  for an example)
         (dx (dfdx noise))
         (dy (dfdy noise)))
    ;; push the normals a bit
    (incf (.x normal) (* 4 dx))
    (incf (.y normal) (* 4 dy))
    ;; and calculate lighting based on distorted normal
    (setf color (shiny-lighting (normalize normal)
                                eye-direction light-direction distance))
    (setf (.a color) 1)))
