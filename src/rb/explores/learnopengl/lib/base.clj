(ns rb.explores.learnopengl.lib.base
  (:import
   [org.lwjgl BufferUtils]
   [org.lwjgl.stb STBImage])
  (:require
   [clojure.java.io :as jio]
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.lwjgl.opengl :refer [use-gl-version]]))

(use-gl-version GL33C)

(defn create-shader
  [type ^String source]
  (let [shader (glCreateShader type)]
    (glShaderSource shader source)
    (glCompileShader shader)
    (if (zero? (glGetShaderi shader GL_COMPILE_STATUS))
      (throw (ex-info "shader compilation failed"
                      {:info-log (glGetShaderInfoLog shader)}))
      shader)))

(defn create-program
  [& shaders]
  (let [program (glCreateProgram)]
    (doseq [shader shaders]
      (glAttachShader program shader))
    (glLinkProgram program)
    (when (zero? (glGetProgrami program GL_LINK_STATUS))
      (throw (ex-info "shader program compilation failed"
                      {:info-log (glGetProgramInfoLog program)})))
    (doseq [shader shaders]
      (glDeleteShader shader))
    program))

(def default-window-hints
  {:client-api :opengl
   :context-version-major 3
   :context-version-minor 3
   :opengl-profile :core})

(defn render
  [{:keys [width height title window-hints
           setup draw step on-key] :as state}]
  (let [state (-> state
                  (update :width #(or % 800))
                  (update :height #(or % 600))
                  (update :title #(or % "LearnOpenGL"))
                  (update :window-hints #(merge default-window-hints %)))]
    (glfw/with-glfw {}
      (glfw/with-gl-window w state
        (let [state* (atom (assoc state :window w))]
          (glfw/swap-interval 1)
          (glfw/set-key-callback
           w (fn [window key scancode action mods]
               (when on-key
                 (swap! state* on-key key scancode action mods))
               (when (and (#{glfw/GLFW_KEY_ESCAPE glfw/GLFW_KEY_Q} key)
                          (= action glfw/GLFW_RELEASE))
                 (glfw/set-window-should-close window true))))
          (glfw/set-framebuffer-size-callback
           w (fn [_ width height]
               (glViewport 0 0 width height)
               (swap! state* assoc
                      :width width
                      :height height)))
          (when setup
            (swap! state* setup))
          (loop [t (glfw/get-time)
                 tprev t]
            (when-not (glfw/window-should-close w)
              (when draw
                (draw @state*))
              (when step
                (let [delta (- t tprev)]
                  (swap! state* step delta)))
              (glfw/swap-buffers w)
              (glfw/poll-events)
              (recur (glfw/get-time) t))))))))

(defn float-buffer
  ^java.nio.FloatBuffer [items]
  (doto (BufferUtils/createFloatBuffer (count items))
    (.put (float-array items))
    (.flip)))

(defn int-buffer
  ^java.nio.IntBuffer [items]
  (doto (BufferUtils/createIntBuffer (count items))
    (.put (int-array items))
    (.flip)))

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

(defn decoded-image?
  [x]
  (and (map? x) (instance? java.nio.ByteBuffer (:data x))))

(defn decode-image
  ([buf desired-channels]
   (if (decoded-image? buf)
     buf
     (do
       (STBImage/stbi_set_flip_vertically_on_load true)
       (let [width (BufferUtils/createIntBuffer 1)
             height (BufferUtils/createIntBuffer 1)
             channels (BufferUtils/createIntBuffer 1)
             data (STBImage/stbi_load_from_memory
                   ^java.nio.ByteBuffer buf
                   width height channels
                   ^int desired-channels)]
         (assert (instance? java.nio.ByteBuffer data) "cannot decode image")
         {:data data
          :width (.get width)
          :height (.get height)
          :channels (.get channels)}))))
  ([buf]
   (decode-image buf 0)))

(defmacro with-image
  [[bind-target img] & body]
  `(let [img# ~img
         decoded?# (decoded-image? img#)
         img# (decode-image img#)
         ~bind-target img#]
     (try
       ~@body
       (finally
         (when-not decoded?#
           (let [^java.nio.ByteBuffer data# (:data img#)]
             (STBImage/stbi_image_free data#)))))))

(defonce images
  (->> {:wooden-container "https://learnopengl.com/img/textures/container.jpg"
        :awesome-face "https://learnopengl.com/img/textures/awesomeface.png"}
       (reduce-kv #(assoc %1 %2 (-> %3 download-bytes decode-image)) {})))
