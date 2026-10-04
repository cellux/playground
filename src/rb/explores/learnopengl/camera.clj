(ns rb.explores.learnopengl.camera
  (:import
   [org.joml Vector3f Vector4f Matrix4f AxisAngle4f]
   [org.lwjgl.system MemoryStack])
  (:require
   [backtick :refer [template]]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.learnopengl.lib.base :refer :all]
   [rb.explores.learnopengl.lib.glsl :as glsl]))

(def ^java.nio.FloatBuffer cube-vertices
  (float-buffer
   [-0.5 , -0.5, -0.5, 0.0, 0.0,
    0.5  , -0.5, -0.5, 1.0, 0.0,
    0.5  , 0.5, -0.5, 1.0, 1.0,
    0.5  , 0.5, -0.5, 1.0, 1.0,
    -0.5 , 0.5, -0.5, 0.0, 1.0,
    -0.5 , -0.5, -0.5, 0.0, 0.0,

    -0.5 , -0.5, 0.5, 0.0, 0.0,
    0.5  , -0.5, 0.5, 1.0, 0.0,
    0.5  , 0.5, 0.5, 1.0, 1.0,
    0.5  , 0.5, 0.5, 1.0, 1.0,
    -0.5 , 0.5, 0.5, 0.0, 1.0,
    -0.5 , -0.5, 0.5, 0.0, 0.0,

    -0.5 , 0.5, 0.5, 1.0, 0.0,
    -0.5 , 0.5, -0.5, 1.0, 1.0,
    -0.5 , -0.5, -0.5, 0.0, 1.0,
    -0.5 , -0.5, -0.5, 0.0, 1.0,
    -0.5 , -0.5, 0.5, 0.0, 0.0,
    -0.5 , 0.5, 0.5, 1.0, 0.0,

    0.5  , 0.5, 0.5, 1.0, 0.0,
    0.5  , 0.5, -0.5, 1.0, 1.0,
    0.5  , -0.5, -0.5, 0.0, 1.0,
    0.5  , -0.5, -0.5, 0.0, 1.0,
    0.5  , -0.5, 0.5, 0.0, 0.0,
    0.5  , 0.5, 0.5, 1.0, 0.0,

    -0.5 , -0.5, -0.5, 0.0, 1.0,
    0.5  , -0.5, -0.5, 1.0, 1.0,
    0.5  , -0.5, 0.5, 1.0, 0.0,
    0.5  , -0.5, 0.5, 1.0, 0.0,
    -0.5 , -0.5, 0.5, 0.0, 0.0,
    -0.5 , -0.5, -0.5, 0.0, 1.0,

    -0.5 , 0.5, -0.5, 0.0, 1.0,
    0.5  , 0.5, -0.5, 1.0, 1.0,
    0.5  , 0.5, 0.5, 1.0, 0.0,
    0.5  , 0.5, 0.5, 1.0, 0.0,
    -0.5 , 0.5, 0.5, 0.0, 0.0,
    -0.5 , 0.5, -0.5, 0.0, 1.0]))

(def cube-positions
  [(Vector3f.  0.0,  0.0,  0.0)
   (Vector3f.  2.0,  5.0, -15.0)
   (Vector3f. -1.5, -2.2, -2.5)
   (Vector3f. -3.8, -2.0, -12.3)
   (Vector3f.  2.4, -0.4, -3.5)
   (Vector3f. -1.7,  3.0, -7.5)
   (Vector3f.  1.3, -2.0, -2.5)
   (Vector3f.  1.5,  2.0, -2.5)
   (Vector3f.  1.5,  0.2, -1.5)
   (Vector3f. -1.3,  1.0, -1.5)])

(defn load-texture
  [tex image-key]
  (glBindTexture GL_TEXTURE_2D tex)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (with-image
    [{:keys [^int width
             ^int height
             ^java.nio.ByteBuffer data
             ^int channels]} (get images image-key)]
    (let [target GL_TEXTURE_2D
          level 0
          internal-format GL_RGB
          border 0
          format (case (int channels)
                   3 GL_RGB
                   4 GL_RGBA)
          type GL_UNSIGNED_BYTE]
      (glTexImage2D target level internal-format
                    width height border format
                    type data)
      (glGenerateMipmap target))))

(defn more-cubes
  []
  (render
   {:setup
    (fn [state]
      (let [tex1 (glGenTextures)
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
            ^int program (create-program
                          (create-shader GL_VERTEX_SHADER vs)
                          (create-shader GL_FRAGMENT_SHADER fs))]
        (glUseProgram program)
        (glUniform1i (glGetUniformLocation program "texture1") 0)
        (load-texture tex1 :wooden-container)
        (glUniform1i (glGetUniformLocation program "texture2") 1)
        (load-texture tex2 :awesome-face)
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER cube-vertices GL_STATIC_DRAW)
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
    (fn [{:keys [vao ^int program tex1 tex2] :as state}]
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
        (let [mat (.mallocFloat stack 16)
              eye (Vector3f. 0 0 3)
              center (Vector3f. 0 0 0)
              up (Vector3f. 0 1 0)]
          (doto (Matrix4f.)
            (.lookAt eye center up)
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
            (let [mat (.mallocFloat stack 16)
                  angle (let [t (glfw/get-time)]
                          (+ angle (* 50 (/ Math/PI 180) (* 3 (Math/sin (+ t angle))))))
                  axis-angle (doto (AxisAngle4f. angle 1.0 0.3 0.5)
                               (.normalize))]
              (doto (Matrix4f.)
                (.translate cube-pos)
                (.rotate axis-angle)
                (.get mat))
              (glUniformMatrix4fv (glGetUniformLocation program "model") false mat)))
          (glDrawArrays GL_TRIANGLES 0 36))))}))

(defn around-cubes
  []
  (render
   {:setup
    (fn [state]
      (let [tex1 (glGenTextures)
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
            ^int program (create-program
                          (create-shader GL_VERTEX_SHADER vs)
                          (create-shader GL_FRAGMENT_SHADER fs))]
        (glUseProgram program)
        (glUniform1i (glGetUniformLocation program "texture1") 0)
        (load-texture tex1 :wooden-container)
        (glUniform1i (glGetUniformLocation program "texture2") 1)
        (load-texture tex2 :awesome-face)
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER cube-vertices GL_STATIC_DRAW)
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
    (fn [{:keys [vao ^int program tex1 tex2] :as state}]
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
        (let [t (glfw/get-time)
              mat (.mallocFloat stack 16)
              radius (+ 8 (* 1.0 (Math/sin (* 2.5 t))))
              eye (let [x (* radius (Math/sin t))
                        y 0
                        z (* radius (Math/cos t))]
                    (Vector3f. x y z))
              center (Vector3f. 0 0 0)
              up (Vector3f. 0 1 0)]
          (doto (Matrix4f.)
            (.lookAt eye center up)
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
          (let [t (glfw/get-time)
                cube-pos (get cube-positions i)
                angle (* 20.0 i)]
            (let [mat (.mallocFloat stack 16)
                  angle (+ angle (* 50 (/ Math/PI 180) (* 3 (Math/sin (+ t angle)))))
                  axis-angle (doto (AxisAngle4f. angle 1.0 0.3 0.5)
                               (.normalize))]
              (doto (Matrix4f.)
                (.translate cube-pos)
                (.rotate axis-angle)
                (.get mat))
              (glUniformMatrix4fv (glGetUniformLocation program "model") false mat)))
          (glDrawArrays GL_TRIANGLES 0 36))))}))

(defn between-cubes
  []
  (render
   {:setup
    (fn [state]
      (let [tex1 (glGenTextures)
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
            ^int program (create-program
                          (create-shader GL_VERTEX_SHADER vs)
                          (create-shader GL_FRAGMENT_SHADER fs))]
        (glUseProgram program)
        (glUniform1i (glGetUniformLocation program "texture1") 0)
        (load-texture tex1 :wooden-container)
        (glUniform1i (glGetUniformLocation program "texture2") 1)
        (load-texture tex2 :awesome-face)
        (glBindVertexArray vao)
        (glBindBuffer GL_ARRAY_BUFFER vbo)
        (glBufferData GL_ARRAY_BUFFER cube-vertices GL_STATIC_DRAW)
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
               :tex2 tex2
               :cam {:pos (Vector3f. 0 0 3)
                     :front (Vector3f. 0 0 -1)
                     :up (Vector3f. 0 1 0)}
               :dir {:pitch 0
                     :yaw 0
                     :roll 0}
               :move-speed 3.0
               :rotate-speed 45)))
    :draw
    (fn [{:keys [vao ^int program tex1 tex2 cam] :as state}]
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
        (let [mat (.mallocFloat stack 16)
              {:keys [^Vector3f pos
                      ^Vector3f front
                      ^Vector3f up]} cam
              center (doto (Vector3f. pos)
                       (.add front))]
          (doto (Matrix4f.)
            (.lookAt pos center up)
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
          (let [t (glfw/get-time)
                cube-pos (get cube-positions i)
                angle (* 20.0 i)]
            (let [mat (.mallocFloat stack 16)
                  angle (+ angle (* 50 (/ Math/PI 180) (* 3 (Math/sin (+ t angle)))))
                  axis-angle (doto (AxisAngle4f. angle 1.0 0.3 0.5)
                               (.normalize))]
              (doto (Matrix4f.)
                (.translate cube-pos)
                (.rotate axis-angle)
                (.get mat))
              (glUniformMatrix4fv (glGetUniformLocation program "model") false mat)))
          (glDrawArrays GL_TRIANGLES 0 36))))
    :step
    (fn [state delta]
      (let [{:keys [window cam dir move-speed rotate-speed]} state
            {:keys [^Vector3f pos
                    ^Vector3f front
                    ^Vector3f up]} cam
            move-speed-delta (float (* move-speed delta))
            rotate-speed-delta (float (* (/ Math/PI 180) rotate-speed delta))
            dir (-> dir
                    (update :pitch +
                            (if (= (glfw/get-key window glfw/GLFW_KEY_UP)
                                   glfw/GLFW_PRESS) rotate-speed-delta 0))
                    (update :pitch -
                            (if (= (glfw/get-key window glfw/GLFW_KEY_DOWN)
                                   glfw/GLFW_PRESS) rotate-speed-delta 0))
                    (update :yaw -
                            (if (= (glfw/get-key window glfw/GLFW_KEY_RIGHT)
                                   glfw/GLFW_PRESS) rotate-speed-delta 0))
                    (update :yaw +
                            (if (= (glfw/get-key window glfw/GLFW_KEY_LEFT)
                                   glfw/GLFW_PRESS) rotate-speed-delta 0)))
            {:keys [pitch yaw roll]} dir
            front (doto (Vector3f. 0 0 -1)
                    (.rotateX pitch)
                    (.rotateY yaw)
                    (.rotateZ roll))
            front-delta (doto (Vector3f. front)
                          (.mul move-speed-delta))
            side-delta (doto (Vector3f. front)
                         (.cross up)
                         (.normalize)
                         (.mul move-speed-delta))]
        (when (= (glfw/get-key window glfw/GLFW_KEY_W) glfw/GLFW_PRESS)
          (.add pos front-delta))
        (when (= (glfw/get-key window glfw/GLFW_KEY_S) glfw/GLFW_PRESS)
          (.sub pos front-delta))
        (when (= (glfw/get-key window glfw/GLFW_KEY_A) glfw/GLFW_PRESS)
          (.sub pos side-delta))
        (when (= (glfw/get-key window glfw/GLFW_KEY_D) glfw/GLFW_PRESS)
          (.add pos side-delta))
        (-> state
            (assoc :dir dir)
            (assoc-in [:cam :front] front))))}))

(comment
  (more-cubes)
  (around-cubes)
  (between-cubes))
