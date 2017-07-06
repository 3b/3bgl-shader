(defsystem :3bgl-shader
  :description "CL-hosted CL-like DSL for generating GLSL"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria bordeaux-threads cl-opengl)
  :serial t
  :components ((:file "package")
               (:file "ir")
               (:file "walker")
               (:file "types")
               (:file "infer")
               (:file "glsl-base")
               (:file "cl-functions")
               (:file "glsl")
               (:file "finalize-inference")
               (:file "printer")
               (:file "compiler")
               (:file "api")
               (:file "old-utils")
               (:file "utils")))
