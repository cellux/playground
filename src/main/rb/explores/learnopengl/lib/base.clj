(ns rb.explores.learnopengl.lib.base
  (:import
   [org.lwjgl BufferUtils])
  (:require
   [rb.explores.lwjgl.glfw :as glfw]
   [rb.explores.lwjgl.opengl :refer [use-gl-version]]))

(use-gl-version GL33C)

(defn create-shader
  [type source]
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

(defn render
  [{:keys [width height title hints
           setup draw update on-key] :as state}]
  (glfw/with-glfw
    (glfw/with-gl-window w
      {:width (or width 800)
       :height (or height 600)
       :title (or title "LearnOpenGL")
       :hints (merge {:client-api :opengl
                      :context-version-major 3
                      :context-version-minor 3
                      :opengl-profile :core}
                     hints)}
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
             (glViewport 0 0 width height)))
        (when setup
          (swap! state* setup))
        (while (not (glfw/window-should-close w))
          (when draw
            (draw @state*))
          (when update
            (swap! state* update))
          (glfw/swap-buffers w)
          (glfw/poll-events))))))

(defn float-buffer
  [items]
  (doto (BufferUtils/createFloatBuffer (count items))
    (.put (float-array items))
    (.flip)))

(defn int-buffer
  [items]
  (doto (BufferUtils/createIntBuffer (count items))
    (.put (int-array items))
    (.flip)))
