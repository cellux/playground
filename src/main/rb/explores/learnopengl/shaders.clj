(ns rb.explores.learnopengl.shaders
  (:import
   [org.lwjgl BufferUtils])
  (:require
   [backtick :refer [template]]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.learnopengl.lib.base :refer :all]
   [rb.explores.learnopengl.lib.glsl :as glsl]))

(defn red-triangle
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
            col [0.5 0.0 0.0 1.0]
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3 :location ~loc)
                           (:out vertexColor vec4)
                           (set! gl_Position (vec4 aPos 1.0))
                           (set! vertexColor (vec4 ~@col)))))
            fs (glsl/fragment-shader
                '((:version 330 core)
                  (:in vertexColor vec4)
                  (:out FragColor vec4)
                  (set! FragColor vertexColor)))
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

(defn triangle-with-uniform-color
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
                           (set! gl_Position (vec4 aPos 1.0)))))
            fs (glsl/fragment-shader
                '((:version 330 core)
                  (:out FragColor vec4)
                  (:uniform ourColor vec4)
                  (set! FragColor ourColor)))
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
               :program program
               :uniform-locations
               {:ourColor (glGetUniformLocation program "ourColor")})))
    :draw
    (fn [{:keys [vao program uniform-locations] :as state}]
      (glClearColor 0.2 0.3 0.3 1.0)
      (glClear GL_COLOR_BUFFER_BIT)
      (glUseProgram program)
      (let [timeValue (glfw/get-time)
            greenValue (+ 0.5 (/ (Math/sin timeValue) 2.0))]
        (glUniform4f (uniform-locations :ourColor) 0 greenValue 0 1))
      (glBindVertexArray vao)
      (glDrawArrays GL_TRIANGLES 0 3))}))

(defn triangle-with-colored-vertices
  []
  (render
   {:setup
    (fn [state]
      (let [vertices (float-buffer
                      [0.5 -0.5 0.0
                       1.0 0.0 0.0

                       -0.5 -0.5 0.0
                       0.0 1.0 0.0
                       
                       0.0 0.5 0.0
                       0.0 0.0 1.0])
            vbo (glGenBuffers)
            vao (glGenVertexArrays)
            locations {:aPos 0
                       :aColor 1}
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3 :location ~(locations :aPos))
                           (:in aColor vec3 :location ~(locations :aColor))
                           (:out ourColor vec3)
                           (set! gl_Position (vec4 aPos 1.0))
                           (set! ourColor aColor))))
            fs (glsl/fragment-shader
                '((:version 330 core)
                  (:out FragColor vec4)
                  (:in ourColor vec3)
                  (set! FragColor (vec4 ourColor 1.0))))
            program (create-program
                     (create-shader GL_VERTEX_SHADER vs)
                     (create-shader GL_FRAGMENT_SHADER fs))]
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
        (let [loc (locations :aPos)]
          (glVertexAttribPointer loc 3 GL_FLOAT false (* 6 Float/BYTES) 0)
          (glEnableVertexAttribArray loc))
        (let [loc (locations :aColor)]
          (glVertexAttribPointer loc 3 GL_FLOAT false (* 6 Float/BYTES) (* 3 Float/BYTES))
          (glEnableVertexAttribArray loc))
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

(comment
  (red-triangle)
  (triangle-with-uniform-color)
  (triangle-with-colored-vertices))
