(ns rb.explores.learnopengl.textures
  (:import
   [org.lwjgl BufferUtils]
   [org.lwjgl.stb STBImage])
  (:require
   [clojure.java.io :as jio]
   [backtick :refer [template]]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.learnopengl.lib.base :refer :all]
   [rb.explores.learnopengl.lib.glsl :as glsl]))

(defn download-bytes
  [url]
  (with-open [in (jio/input-stream url)
              out (java.io.ByteArrayOutputStream.)]
    (jio/copy in out)
    (let [ba (.toByteArray out)
          size (.size out)
          buf (BufferUtils/createByteBuffer size)]
      (.put buf ba 0 size)
      (.flip buf)
      buf)))

(defonce images
  (->> {:wooden-container "https://learnopengl.com/img/textures/container.jpg"
        :awesome-face "https://learnopengl.com/img/textures/awesomeface.png"}
       (reduce-kv #(assoc %1 %2 (download-bytes %3)) {})))

(defn decode-image
  ([buf desired-channels]
   (STBImage/stbi_set_flip_vertically_on_load true)
   (let [width (BufferUtils/createIntBuffer 1)
         height (BufferUtils/createIntBuffer 1)
         channels (BufferUtils/createIntBuffer 1)
         data (STBImage/stbi_load_from_memory buf width height channels desired-channels)]
     (assert data "cannot decode image")
     {:data data
      :width (.get width)
      :height (.get height)
      :channels (.get channels)}))
  ([buf]
   (decode-image buf 0)))

(defmacro with-decoded-image
  [[bind-target img] & body]
  `(let [img# ~img
         ~bind-target img#]
     (try
       ~@body
       (finally
         (STBImage/stbi_image_free (:data img#))))))

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
            locations {:aPos 0
                       :aColor 1
                       :aTexCoord 2}
            vs (glsl/vertex-shader
                (template ((:version 330 core)
                           (:in aPos vec3 :location ~(locations :aPos))
                           (:in aColor vec3 :location ~(locations :aColor))
                           (:in aTexCoord vec2 :location ~(locations :aTexCoord))
                           (:out ourColor vec3)
                           (:out texCoord vec2)
                           (set! gl_Position (vec4 aPos 1.0))
                           (set! ourColor aColor)
                           (set! texCoord aTexCoord))))
            fs (glsl/fragment-shader
                (template ((:version 330 core)
                           (:in ourColor vec3)
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
                           (with-decoded-image
                             [{:keys [width height data channels]} (decode-image (get images image-key))]
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
        (glUniform1i (glGetUniformLocation program "texture2") 1)
        (load-texture tex1 :wooden-container)
        (load-texture tex2 :awesome-face)
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER vertices GL_STATIC_DRAW)
        (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
        (glBufferData GL_ELEMENT_ARRAY_BUFFER indices GL_STATIC_DRAW)
        (let [stride (* 8 Float/BYTES)]
          (let [loc (locations :aPos)]
            (glVertexAttribPointer loc 3 GL_FLOAT false stride 0)
            (glEnableVertexAttribArray loc))
          (let [loc (locations :aColor)]
            (glVertexAttribPointer loc 3 GL_FLOAT false stride (* 3 Float/BYTES))
            (glEnableVertexAttribArray loc))
          (let [loc (locations :aTexCoord)]
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

(comment
  (textured-rectangle))
