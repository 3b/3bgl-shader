(defsystem :3bgl-shader-example
  :depends-on (3bgl-shader mathkit cl-glut cl-glu)
  :serial t
  :components ((:file "example-shaders")
               (:file "example")))
