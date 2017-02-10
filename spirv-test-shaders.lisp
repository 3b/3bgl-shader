;(delete-package '#:3bgl-shader-spirv-test-shaders)
(defpackage #:3bgl-shader-spirv-test-shaders
  (:use #:3bgl-glsl/cl)
  (:export
   #:fragment))
(in-package #:3bgl-shader-spirv-test-shaders)


(input in-color :uvec2 :stage :fragment)

(output color :u64vec2 :stage :fragment)

(uniform x :uint16 :stage :fragment)
(input fv2 :vec2 :stage :fragment)
(uniform f :float :stage :fragment)

(defun fragment ()
  (declare (values))
  (memory-barrier)
  ;;(incf x)
  ;;(setf color (- in-color))
  (setf fv2 (mod fv2 (vec2 1)))
  )

"#version 450
uniform float x;

in vec4 inColor;

out vec4 color;

void main () {
  color = inColor * x;
}

"
