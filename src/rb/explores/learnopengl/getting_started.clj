(ns rb.explores.learnopengl.getting-started
  (:require
   [backtick :refer [template]]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.learnopengl.lib.base :refer :all]
   [rb.explores.learnopengl.lib.glsl :as glsl]))

(defn hello-window
  []
  (render
   {:draw
    (fn [state]
      (glClearColor 0.2 0.3 0.3 1.0)
      (glClear GL_COLOR_BUFFER_BIT))}))

(defn hello-triangle
  []
  (render
   {:setup
    (fn [state]
      (let [vertices (float-buffer [-0.5 -0.5 0.0
                                    0.5 -0.5 0.0
                                    0.0 0.5 0.0])
            vbo (glGenBuffers)
            vao (glGenVertexArrays)
            loc 0
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3 :location ~loc)
                           (set! gl_Position (vec4 aPos.x aPos.y aPos.z 1.0)))))
            col [1.0 0.5 0.2 1.0]
            fs (glsl/fragment-shader
                (template ((:version 330 core)
                           (:out FragColor vec4)
                           (set! FragColor (vec4 ~@col)))))
            program (create-program
                     (create-shader GL_VERTEX_SHADER vs)
                     (create-shader GL_FRAGMENT_SHADER fs))]
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
        (glVertexAttribPointer loc 3 GL_FLOAT false (* 3 Float/BYTES) 0)
        (glEnableVertexAttribArray 0)
        (assoc state
               :vao vao
               :program program)))
    :draw
    (fn [{:keys [vao program] :as state}]
      (glClearColor 0.2 0.3 0.3 1.0)
      (glClear GL_COLOR_BUFFER_BIT)
      (glUseProgram program)
      (glBindVertexArray vao)
      (glDrawArrays GL_TRIANGLES 0 3))}))

(defn hello-rectangle
  []
  (render
   {:setup
    (fn [state]
      (let [vertices (float-buffer [0.5 0.5 0.0   ; top right
                                    0.5 -0.5 0.0  ; bottom right
                                    -0.5 -0.5 0.0 ; bottom left
                                    -0.5 0.5 0.0  ; top left
                                    ])
            indices (int-buffer [0 1 3
                                 1 2 3])
            vbo (glGenBuffers)
            ebo (glGenBuffers)
            vao (glGenVertexArrays)
            loc 0
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3 :location ~loc)
                           (set! gl_Position (vec4 aPos.x aPos.y aPos.z 1.0)))))
            col [1.0 0.5 0.2 1.0]
            fs (glsl/fragment-shader
                (template ((:version 330 core)
                           (:out FragColor vec4)
                           (set! FragColor (vec4 ~@col)))))
            program (create-program
                     (create-shader GL_VERTEX_SHADER vs)
                     (create-shader GL_FRAGMENT_SHADER fs))]
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
        (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
        (glBufferData GL_ELEMENT_ARRAY_BUFFER indices GL_STATIC_DRAW)
        (glVertexAttribPointer loc 3 GL_FLOAT false (* 3 Float/BYTES) 0)
        (glEnableVertexAttribArray 0)
        (assoc state
               :vao vao
               :program program
               :polygon-mode GL_FILL)))
    :draw
    (fn [{:keys [vao program polygon-mode] :as state}]
      (glClearColor 0.2 0.3 0.3 1.0)
      (glClear GL_COLOR_BUFFER_BIT)
      (glPolygonMode GL_FRONT_AND_BACK polygon-mode)
      (glUseProgram program)
      (glBindVertexArray vao)
      (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT 0))
    :on-key
    (fn [state key scancode action mods]
      (if (and (= action glfw/GLFW_PRESS)
               (= key glfw/GLFW_KEY_P))
        (update state :polygon-mode #(if (= GL_FILL %)
                                       GL_LINE GL_FILL))
        state))}))

(comment
  (hello-window)
  (hello-triangle)
  (hello-rectangle))
