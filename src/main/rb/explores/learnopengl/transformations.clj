(ns rb.explores.learnopengl.transformations
  (:import
   [org.joml Vector3f Vector4f Matrix4f]
   [org.lwjgl BufferUtils]
   [org.lwjgl.system MemoryStack]
   [org.lwjgl.stb STBImage])
  (:require
   [clojure.test :refer [is deftest]]
   [backtick :refer [template]]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.learnopengl.lib.base :refer :all]
   [rb.explores.learnopengl.lib.glsl :as glsl]))

(deftest test1
  (let [vec (Vector4f. 1 0 0 1)]
    (doto (Matrix4f.)
      (.translate 1 1 0)
      (.transform vec))
    (is (= vec (Vector4f. 2 1 0 1)))))

(deftest test2
  (let [vec (Vector4f. 1 0 0 1)] 
   (doto (Matrix4f.)
     (.translate 2 5 3)
     (.rotateZ (/ Math/PI 2))
     (.scale 0.5)
     (.transform vec))
    (is (= vec (Vector4f. 2 5.5 3 1)))))

(deftest test3
  (let [vec (Vector4f. 1 0 0 1)] 
    (doto (Matrix4f.)
      (.rotateZ (/ Math/PI 2))
      (.scale 0.5)
      (.translate 6 0 0)
      (.transform vec))
    (is (= vec (Vector4f. 0 3.5 0 1)))))

(defn textured-rectangle
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
                           (:uniform transform mat4)
                           (set! gl_Position (* transform (vec4 aPos 1.0)))
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
        (let [mat (BufferUtils/createFloatBuffer 16)]
          (doto (Matrix4f.)
            (.rotateZ (/ Math/PI 2))
            (.scale 0.5)
            (.get mat))
          (glUniformMatrix4fv (glGetUniformLocation program "transform") false mat))
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
      (glBindVertexArray vao)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D tex1)
      (glActiveTexture GL_TEXTURE1)
      (glBindTexture GL_TEXTURE_2D tex2)
      (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT 0))}))

(defn rotating-rectangle
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
                           (:uniform transform mat4)
                           (set! gl_Position (* transform (vec4 aPos 1.0)))
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
            (.translate 0.5 -0.5 0)
            (.rotateZ (glfw/get-time))
            (.get mat))
          (glUniformMatrix4fv (glGetUniformLocation program "transform") false mat)))
      (glBindVertexArray vao)
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D tex1)
      (glActiveTexture GL_TEXTURE1)
      (glBindTexture GL_TEXTURE_2D tex2)
      (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT 0))}))

(comment
  (textured-rectangle)
  (rotating-rectangle))
