(ns rb.explores.learnopengl.coordinate-systems
  (:import
   [org.joml Vector3f Vector4f Matrix4f]
   [org.lwjgl.system MemoryStack])
  (:require
   [clojure.test :refer [is deftest]]
   [backtick :refer [template]]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.learnopengl.lib.base :refer :all]
   [rb.explores.learnopengl.lib.glsl :as glsl]))

(defn rectangle-on-the-floor
  []
  (render
   {:setup
    (fn [state]
      (let [vertices (float-buffer [0.5, 0.5, 0.0,
                                    1.0, 0.0, 0.0,
                                    1.0, 1.0,
                                    
                                    0.5, -0.5, 0.0,
                                    0.0, 1.0, 0.0,
                                    1.0, 0.0,
                                    
                                    -0.5, -0.5, 0.0,
                                    0.0, 0.0, 1.0,
                                    0.0, 0.0,
                                    
                                    -0.5, 0.5, 0.0,
                                    1.0, 1.0, 0.0,
                                    0.0, 1.0])
            indices (int-buffer [0 1 3
                                 1 2 3])
            tex1 (glGenTextures)
            tex2 (glGenTextures)
            vbo (glGenBuffers)
            ebo (glGenBuffers)
            vao (glGenVertexArrays)
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3)
                           (:in aTexCoord vec2)
                           (:out texCoord vec2)
                           (:uniform model mat4)
                           (:uniform view mat4)
                           (:uniform projection mat4)
                           (set! gl_Position (* projection view model (vec4 aPos 1.0)))
                           (set! texCoord aTexCoord))))
            fs (glsl/fragment-shader
                (template ((:version 330 core)
                           (:in texCoord vec2)
                           (:uniform texture1 sampler2D)
                           (:uniform texture2 sampler2D)
                           (:out FragColor vec4)
                           (set! FragColor (mix (texture texture1 texCoord)
                                                (texture texture2 texCoord)
                                                0.2)))))
            program (create-program
                     (create-shader GL_VERTEX_SHADER vs)
                     (create-shader GL_FRAGMENT_SHADER fs))
            load-texture (fn [tex image-key]
                           (glBindTexture GL_TEXTURE_2D tex)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                           (with-image
                             [{:keys [width height data channels]} (get images image-key)]
                             (let [target GL_TEXTURE_2D
                                   level 0
                                   internal-format GL_RGB
                                   border 0
                                   format (case channels
                                            3 GL_RGB
                                            4 GL_RGBA)
                                   type GL_UNSIGNED_BYTE]
                               (glTexImage2D target level internal-format width height border format type data)
                               (glGenerateMipmap target))))]
        (glUseProgram program)
        (glUniform1i (glGetUniformLocation program "texture1") 0)
        (load-texture tex1 :wooden-container)
        (glUniform1i (glGetUniformLocation program "texture2") 1)
        (load-texture tex2 :awesome-face)
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
        (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
        (glBufferData GL_ELEMENT_ARRAY_BUFFER indices GL_STATIC_DRAW)
        (let [stride (* 8 Float/BYTES)]
          (let [loc (glGetAttribLocation program "aPos")]
            (glVertexAttribPointer loc 3 GL_FLOAT false stride 0)
            (glEnableVertexAttribArray loc))
          (let [loc (glGetAttribLocation program "aTexCoord")]
            (glVertexAttribPointer loc 2 GL_FLOAT false stride (* 6 Float/BYTES))
            (glEnableVertexAttribArray loc)))
        (assoc state
               :vao vao
               :program program
               :tex1 tex1
               :tex2 tex2)))
    :draw
    (fn [{:keys [vao program tex1 tex2] :as state}]
      (glClearColor 0.2 0.3 0.3 1.0)
      (glClear GL_COLOR_BUFFER_BIT)
      (glUseProgram program)
      (with-open [stack (MemoryStack/stackPush)]
        (let [mat (.mallocFloat stack 16)]
          (doto (Matrix4f.)
            (.rotateX (* -55 (/ Math/PI 180)))
            (.get mat))
          (glUniformMatrix4fv (glGetUniformLocation program "model") false mat))
        (let [mat (.mallocFloat stack 16)]
          (doto (Matrix4f.)
            (.translate 0 0 -3)
            (.get mat))
          (glUniformMatrix4fv (glGetUniformLocation program "view") false mat))
        (let [mat (.mallocFloat stack 16)]
          (let [fovy (* (/ Math/PI 180) 45)
                aspect 800/600
                znear 0.1
                zfar 100]
            (doto (Matrix4f.)
              (.perspective fovy aspect znear zfar)
              (.get mat)))
          (glUniformMatrix4fv (glGetUniformLocation program "projection") false mat)))
      (glBindVertexArray vao)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D tex1)
      (glActiveTexture GL_TEXTURE1)
      (glBindTexture GL_TEXTURE_2D tex2)
      (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT 0))}))

(defn rotating-cube
  []
  (render
   {:setup
    (fn [state]
      (let [vertices (float-buffer [-0.5 , -0.5, -0.5,  0.0, 0.0,
                                    0.5  , -0.5, -0.5,  1.0, 0.0,
                                    0.5  ,  0.5, -0.5,  1.0, 1.0,
                                    0.5  ,  0.5, -0.5,  1.0, 1.0,
                                    -0.5 ,  0.5, -0.5,  0.0, 1.0,
                                    -0.5 , -0.5, -0.5,  0.0, 0.0,

                                    -0.5 , -0.5,  0.5,  0.0, 0.0,
                                    0.5  , -0.5,  0.5,  1.0, 0.0,
                                    0.5  ,  0.5,  0.5,  1.0, 1.0,
                                    0.5  ,  0.5,  0.5,  1.0, 1.0,
                                    -0.5 ,  0.5,  0.5,  0.0, 1.0,
                                    -0.5 , -0.5,  0.5,  0.0, 0.0,

                                    -0.5 ,  0.5,  0.5,  1.0, 0.0,
                                    -0.5 ,  0.5, -0.5,  1.0, 1.0,
                                    -0.5 , -0.5, -0.5,  0.0, 1.0,
                                    -0.5 , -0.5, -0.5,  0.0, 1.0,
                                    -0.5 , -0.5,  0.5,  0.0, 0.0,
                                    -0.5 ,  0.5,  0.5,  1.0, 0.0,

                                    0.5  ,  0.5,  0.5,  1.0, 0.0,
                                    0.5  ,  0.5, -0.5,  1.0, 1.0,
                                    0.5  , -0.5, -0.5,  0.0, 1.0,
                                    0.5  , -0.5, -0.5,  0.0, 1.0,
                                    0.5  , -0.5,  0.5,  0.0, 0.0,
                                    0.5  ,  0.5,  0.5,  1.0, 0.0,

                                    -0.5 , -0.5, -0.5,  0.0, 1.0,
                                    0.5  , -0.5, -0.5,  1.0, 1.0,
                                    0.5  , -0.5,  0.5,  1.0, 0.0,
                                    0.5  , -0.5,  0.5,  1.0, 0.0,
                                    -0.5 , -0.5,  0.5,  0.0, 0.0,
                                    -0.5 , -0.5, -0.5,  0.0, 1.0,
                                    
                                    -0.5 ,  0.5, -0.5,  0.0, 1.0,
                                    0.5  ,  0.5, -0.5,  1.0, 1.0,
                                    0.5  ,  0.5,  0.5,  1.0, 0.0,
                                    0.5  ,  0.5,  0.5,  1.0, 0.0,
                                    -0.5 ,  0.5,  0.5,  0.0, 0.0,
                                    -0.5 ,  0.5, -0.5,  0.0, 1.0])
            tex1 (glGenTextures)
            tex2 (glGenTextures)
            vbo (glGenBuffers)
            vao (glGenVertexArrays)
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3)
                           (:in aTexCoord vec2)
                           (:out texCoord vec2)
                           (:uniform model mat4)
                           (:uniform view mat4)
                           (:uniform projection mat4)
                           (set! gl_Position (* projection view model (vec4 aPos 1.0)))
                           (set! texCoord aTexCoord))))
            fs (glsl/fragment-shader
                (template ((:version 330 core)
                           (:in texCoord vec2)
                           (:uniform texture1 sampler2D)
                           (:uniform texture2 sampler2D)
                           (:out FragColor vec4)
                           (set! FragColor (mix (texture texture1 texCoord)
                                                (texture texture2 texCoord)
                                                0.2)))))
            program (create-program
                     (create-shader GL_VERTEX_SHADER vs)
                     (create-shader GL_FRAGMENT_SHADER fs))
            load-texture (fn [tex image-key]
                           (glBindTexture GL_TEXTURE_2D tex)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
                           (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                           (with-image
                             [{:keys [width height data channels]} (get images image-key)]
                             (let [target GL_TEXTURE_2D
                                   level 0
                                   internal-format GL_RGB
                                   border 0
                                   format (case channels
                                            3 GL_RGB
                                            4 GL_RGBA)
                                   type GL_UNSIGNED_BYTE]
                               (glTexImage2D target level internal-format width height border format type data)
                               (glGenerateMipmap target))))]
        (glUseProgram program)
        (glUniform1i (glGetUniformLocation program "texture1") 0)
        (load-texture tex1 :wooden-container)
        (glUniform1i (glGetUniformLocation program "texture2") 1)
        (load-texture tex2 :awesome-face)
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
        (let [stride (* 5 Float/BYTES)]
          (let [loc (glGetAttribLocation program "aPos")]
            (glVertexAttribPointer loc 3 GL_FLOAT false stride 0)
            (glEnableVertexAttribArray loc))
          (let [loc (glGetAttribLocation program "aTexCoord")]
            (glVertexAttribPointer loc 2 GL_FLOAT false stride (* 3 Float/BYTES))
            (glEnableVertexAttribArray loc)))
        (assoc state
               :vao vao
               :program program
               :tex1 tex1
               :tex2 tex2)))
    :draw
    (fn [{:keys [vao program tex1 tex2] :as state}]
      (glEnable GL_DEPTH_TEST)
      (glClearColor 0.2 0.3 0.3 1.0)
      (glClear (bit-or GL_COLOR_BUFFER_BIT
                       GL_DEPTH_BUFFER_BIT))
      (glUseProgram program)
      (with-open [stack (MemoryStack/stackPush)]
        (let [mat (.mallocFloat stack 16)]
          (doto (Matrix4f.)
            (.rotate (* 50 (/ Math/PI 180) (glfw/get-time))
                     0.5 1.0 0.0)
            (.get mat))
          (glUniformMatrix4fv (glGetUniformLocation program "model") false mat))
        (let [mat (.mallocFloat stack 16)]
          (doto (Matrix4f.)
            (.translate 0 0 -3)
            (.get mat))
          (glUniformMatrix4fv (glGetUniformLocation program "view") false mat))
        (let [mat (.mallocFloat stack 16)]
          (let [fovy (* (/ Math/PI 180) 45)
                aspect (/ (:width state) (:height state))
                znear 0.1
                zfar 100]
            (doto (Matrix4f.)
              (.perspective fovy aspect znear zfar)
              (.get mat)))
          (glUniformMatrix4fv (glGetUniformLocation program "projection") false mat)))
      (glBindVertexArray vao)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D tex1)
      (glActiveTexture GL_TEXTURE1)
      (glBindTexture GL_TEXTURE_2D tex2)
      (glDrawArrays GL_TRIANGLES 0 36))}))

(defn more-cubes
  []
  (let [cube-positions [(Vector3f.  0.0,  0.0,  0.0)
                        (Vector3f.  2.0,  5.0, -15.0)
                        (Vector3f. -1.5, -2.2, -2.5)
                        (Vector3f. -3.8, -2.0, -12.3)
                        (Vector3f.  2.4, -0.4, -3.5)
                        (Vector3f. -1.7,  3.0, -7.5)
                        (Vector3f.  1.3, -2.0, -2.5)
                        (Vector3f.  1.5,  2.0, -2.5)
                        (Vector3f.  1.5,  0.2, -1.5)
                        (Vector3f. -1.3,  1.0, -1.5)]]
    (render
     {:setup
      (fn [state]
        (let [vertices (float-buffer [-0.5 , -0.5, -0.5,  0.0, 0.0,
                                      0.5  , -0.5, -0.5,  1.0, 0.0,
                                      0.5  ,  0.5, -0.5,  1.0, 1.0,
                                      0.5  ,  0.5, -0.5,  1.0, 1.0,
                                      -0.5 ,  0.5, -0.5,  0.0, 1.0,
                                      -0.5 , -0.5, -0.5,  0.0, 0.0,

                                      -0.5 , -0.5,  0.5,  0.0, 0.0,
                                      0.5  , -0.5,  0.5,  1.0, 0.0,
                                      0.5  ,  0.5,  0.5,  1.0, 1.0,
                                      0.5  ,  0.5,  0.5,  1.0, 1.0,
                                      -0.5 ,  0.5,  0.5,  0.0, 1.0,
                                      -0.5 , -0.5,  0.5,  0.0, 0.0,

                                      -0.5 ,  0.5,  0.5,  1.0, 0.0,
                                      -0.5 ,  0.5, -0.5,  1.0, 1.0,
                                      -0.5 , -0.5, -0.5,  0.0, 1.0,
                                      -0.5 , -0.5, -0.5,  0.0, 1.0,
                                      -0.5 , -0.5,  0.5,  0.0, 0.0,
                                      -0.5 ,  0.5,  0.5,  1.0, 0.0,

                                      0.5  ,  0.5,  0.5,  1.0, 0.0,
                                      0.5  ,  0.5, -0.5,  1.0, 1.0,
                                      0.5  , -0.5, -0.5,  0.0, 1.0,
                                      0.5  , -0.5, -0.5,  0.0, 1.0,
                                      0.5  , -0.5,  0.5,  0.0, 0.0,
                                      0.5  ,  0.5,  0.5,  1.0, 0.0,

                                      -0.5 , -0.5, -0.5,  0.0, 1.0,
                                      0.5  , -0.5, -0.5,  1.0, 1.0,
                                      0.5  , -0.5,  0.5,  1.0, 0.0,
                                      0.5  , -0.5,  0.5,  1.0, 0.0,
                                      -0.5 , -0.5,  0.5,  0.0, 0.0,
                                      -0.5 , -0.5, -0.5,  0.0, 1.0,
                                    
                                      -0.5 ,  0.5, -0.5,  0.0, 1.0,
                                      0.5  ,  0.5, -0.5,  1.0, 1.0,
                                      0.5  ,  0.5,  0.5,  1.0, 0.0,
                                      0.5  ,  0.5,  0.5,  1.0, 0.0,
                                      -0.5 ,  0.5,  0.5,  0.0, 0.0,
                                      -0.5 ,  0.5, -0.5,  0.0, 1.0])
              tex1 (glGenTextures)
              tex2 (glGenTextures)
              vbo (glGenBuffers)
              vao (glGenVertexArrays)
              vs (glsl/vertex-shader
                  (template ((:version 330 core)
                             (:in aPos vec3)
                             (:in aTexCoord vec2)
                             (:out texCoord vec2)
                             (:uniform model mat4)
                             (:uniform view mat4)
                             (:uniform projection mat4)
                             (set! gl_Position (* projection view model (vec4 aPos 1.0)))
                             (set! texCoord aTexCoord))))
              fs (glsl/fragment-shader
                  (template ((:version 330 core)
                             (:in texCoord vec2)
                             (:uniform texture1 sampler2D)
                             (:uniform texture2 sampler2D)
                             (:out FragColor vec4)
                             (set! FragColor (mix (texture texture1 texCoord)
                                                  (texture texture2 texCoord)
                                                  0.2)))))
              program (create-program
                       (create-shader GL_VERTEX_SHADER vs)
                       (create-shader GL_FRAGMENT_SHADER fs))
              load-texture (fn [tex image-key]
                             (glBindTexture GL_TEXTURE_2D tex)
                             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
                             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
                             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                             (with-image
                               [{:keys [width height data channels]} (get images image-key)]
                               (let [target GL_TEXTURE_2D
                                     level 0
                                     internal-format GL_RGB
                                     border 0
                                     format (case channels
                                              3 GL_RGB
                                              4 GL_RGBA)
                                     type GL_UNSIGNED_BYTE]
                                 (glTexImage2D target level internal-format width height border format type data)
                                 (glGenerateMipmap target))))]
          (glUseProgram program)
          (glUniform1i (glGetUniformLocation program "texture1") 0)
          (load-texture tex1 :wooden-container)
          (glUniform1i (glGetUniformLocation program "texture2") 1)
          (load-texture tex2 :awesome-face)
          (glBindVertexArray vao)
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
          (let [stride (* 5 Float/BYTES)]
            (let [loc (glGetAttribLocation program "aPos")]
              (glVertexAttribPointer loc 3 GL_FLOAT false stride 0)
              (glEnableVertexAttribArray loc))
            (let [loc (glGetAttribLocation program "aTexCoord")]
              (glVertexAttribPointer loc 2 GL_FLOAT false stride (* 3 Float/BYTES))
              (glEnableVertexAttribArray loc)))
          (assoc state
                 :vao vao
                 :program program
                 :tex1 tex1
                 :tex2 tex2)))
      :draw
      (fn [{:keys [vao program tex1 tex2] :as state}]
        (glEnable GL_DEPTH_TEST)
        (glClearColor 0.2 0.3 0.3 1.0)
        (glClear (bit-or GL_COLOR_BUFFER_BIT
                         GL_DEPTH_BUFFER_BIT))
        (glUseProgram program)
        (glBindVertexArray vao)
        (glActiveTexture GL_TEXTURE0)
        (glBindTexture GL_TEXTURE_2D tex1)
        (glActiveTexture GL_TEXTURE1)
        (glBindTexture GL_TEXTURE_2D tex2)
        (with-open [stack (MemoryStack/stackPush)]
          (let [mat (.mallocFloat stack 16)]
            (doto (Matrix4f.)
              (.translate 0 0 -3)
              (.get mat))
            (glUniformMatrix4fv (glGetUniformLocation program "view") false mat))
          (let [mat (.mallocFloat stack 16)]
            (let [fovy (* (/ Math/PI 180) 45)
                  aspect (/ (:width state) (:height state))
                  znear 0.1
                  zfar 100]
              (doto (Matrix4f.)
                (.perspective fovy aspect znear zfar)
                (.get mat)))
            (glUniformMatrix4fv (glGetUniformLocation program "projection") false mat))
          (dotimes [i (count cube-positions)]
            (let [cube-pos (get cube-positions i)
                  angle (* 20.0 i)]
              (let [mat (.mallocFloat stack 16)]
                (doto (Matrix4f.)
                  (.translate cube-pos)
                  (.rotate (let [t (glfw/get-time)]
                             (+ angle (* 50 (/ Math/PI 180) (* 3 (Math/sin (+ t angle))))))
                           1.0 0.3 0.5)
                  (.get mat))
                (glUniformMatrix4fv (glGetUniformLocation program "model") false mat)))
            (glDrawArrays GL_TRIANGLES 0 36))))})))

(comment
  (rectangle-on-the-floor)
  (rotating-cube)
  (more-cubes))
