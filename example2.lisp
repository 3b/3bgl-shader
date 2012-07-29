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

(uniform-block transforms ()
    ((mvp :mat4)
      (normal-matrix :mat4)))

(uniform-block bones ()
    ((bone-matrices (:mat4 32))))

(bind-interface interpolated-vars in)

(defun main ()
  (let ((pos
          (+ (* (aref bone-matrices (.x bone-indices)) vertex (.x bone-weights))
             (* (aref bone-matrices (.y bone-indices)) vertex (.y bone-weights))
             (* (aref bone-matrices (.z bone-indices)) vertex (.z bone-weights))
             (* (aref bone-matrices (.w bone-indices)) vertex (.w bone-weights)))))
    (setf (.position out) (* mvp pos)
          (.normal out) (* normal-matrix normal)
          (.tangent out) (* normal-matrix tangent)
          (.bitangent out) (* normal-matrix bitangent)
          (.uv out) uv
          (.color out) (vec4 1.0 1.0 1.0 1.0))))
