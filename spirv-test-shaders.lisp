(defpackage #:3bgl-shader-spirv-test-shaders
  (:use #:3bgl-glsl/cl)
  (:export
   #:fragment))
(in-package #:3bgl-shader-spirv-test-shaders)


(input in-color :vec4 :stage :fragment)

(output color :vec4 :stage :fragment)

(defun fragment ()
  (declare (values))
  (setf color in-color))

"#version 450
out vec4 color;

in vec4 inColor;

void main () {
  color = inColor;
}

"
