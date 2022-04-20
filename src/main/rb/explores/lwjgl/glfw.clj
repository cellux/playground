(ns rb.explores.lwjgl.glfw
  (:require
   [clojure.reflect :refer [reflect]])
  (:import
   [org.lwjgl Version BufferUtils PointerBuffer]
   [org.lwjgl.glfw
    GLFW Callbacks
    GLFWErrorCallbackI
    GLFWKeyCallbackI
    GLFWFramebufferSizeCallbackI]
   [org.lwjgl.opengl GL]))

(defn gen-defs
  [glfw-class]
  (let [{:keys [members]} (reflect glfw-class)]
    (for [m members :when (and ((:flags m) :public)
                               (= (:type m) 'int))]
      `(def ~(:name m)
         ~(symbol (name (:declaring-class m))
                  (name (:name m)))))))

(defmacro gen-defs*
  []
  `(do
     ~@(gen-defs GLFW)))

(gen-defs*)

(defn get-version
  []
  (let [major (BufferUtils/createIntBuffer 1)
        minor (BufferUtils/createIntBuffer 1)
        rev (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetVersion major minor rev)
    [(.get major)
     (.get minor)
     (.get rev)]))

(defn get-version-string
  []
  (GLFW/glfwGetVersionString))

(defn get-error-descriptor
  []
  (let [pb (PointerBuffer/allocateDirect 1)
        error-code (GLFW/glfwGetError pb)]
    (merge {:error-code error-code}
           (when-not (zero? error-code)
             (let [p (.get pb 0)]
               (when-not (zero? p)
                 {:error-description (.getStringUTF8 pb)}))))))

(defmacro fail-if-result=
  [value form]
  `(let [result# ~form]
     (when (= result# ~value)
       (throw (ex-info "glfw call failed"
                       (merge {:form '~form
                               :result result#}
                              (get-error-descriptor)))))
     result#))

(defn set-error-callback
  [callback]
  (let [cbfun (reify GLFWErrorCallbackI
                (invoke [this error-code error-description]
                  (callback error-code error-description)))]
    (GLFW/glfwSetErrorCallback cbfun)))

(defn get-primary-monitor
  []
  (GLFW/glfwGetPrimaryMonitor))

(defn get-monitor-physical-size
  [^long monitor]
  (let [width (BufferUtils/createIntBuffer 1)
        height (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetMonitorPhysicalSize monitor width height)
    [(.get width) (.get height)]))

(defn get-monitor-pos
  [^long monitor]
  (let [width (BufferUtils/createIntBuffer 1)
        height (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetMonitorPos monitor width height)
    [(.get width) (.get height)]))

(defn get-monitor-name
  [^long monitor]
  (GLFW/glfwGetMonitorName monitor))

(defn- vidmode->map
  [^org.lwjgl.glfw.GLFWVidMode vidmode]
  {:mode vidmode
   :blue-bits (.blueBits vidmode)
   :green-bits (.greenBits vidmode)
   :red-bits (.redBits vidmode)
   :width (.width vidmode)
   :height (.height vidmode)})

(defn get-video-mode
  [monitor]
  (vidmode->map (GLFW/glfwGetVideoMode monitor)))

(defn get-video-modes
  [monitor]
  (->> (GLFW/glfwGetVideoModes monitor)
       (mapv vidmode->map)))

(def window-hints
  {:resizable GLFW/GLFW_RESIZABLE
   :visible GLFW/GLFW_VISIBLE
   :decorated GLFW/GLFW_DECORATED
   :focused GLFW/GLFW_FOCUSED
   :auto-iconify GLFW/GLFW_AUTO_ICONIFY
   :floating GLFW/GLFW_FLOATING
   :maximized GLFW/GLFW_MAXIMIZED
   :center-cursor GLFW/GLFW_CENTER_CURSOR
   :transparent-framebuffer GLFW/GLFW_TRANSPARENT_FRAMEBUFFER
   :focus-on-show GLFW/GLFW_FOCUS_ON_SHOW
   :scale-to-monitor GLFW/GLFW_SCALE_TO_MONITOR

   :red-bits GLFW/GLFW_RED_BITS
   :green-bits GLFW/GLFW_GREEN_BITS
   :blue-bits GLFW/GLFW_BLUE_BITS
   :alpha-bits GLFW/GLFW_ALPHA_BITS
   :depth-bits GLFW/GLFW_DEPTH_BITS
   :stencil-bits GLFW/GLFW_STENCIL_BITS
  
   :stereo GLFW/GLFW_STEREO
   :samples GLFW/GLFW_SAMPLES
   :srgb-capable GLFW/GLFW_SRGB_CAPABLE
   :doublebuffer GLFW/GLFW_DOUBLEBUFFER

   :refresh-rate GLFW/GLFW_REFRESH_RATE
   :client-api GLFW/GLFW_CLIENT_API

   :context-creation-api GLFW/GLFW_CONTEXT_CREATION_API
   :context-version-major GLFW/GLFW_CONTEXT_VERSION_MAJOR
   :context-version-minor GLFW/GLFW_CONTEXT_VERSION_MINOR

   :opengl-forward-compat GLFW/GLFW_OPENGL_FORWARD_COMPAT
   :opengl-debug-context GLFW/GLFW_OPENGL_DEBUG_CONTEXT
   :opengl-profile GLFW/GLFW_OPENGL_PROFILE

   :context-robustness GLFW/GLFW_CONTEXT_ROBUSTNESS
   :context-release-behavior GLFW/GLFW_CONTEXT_RELEASE_BEHAVIOR
   :context-no-error GLFW/GLFW_CONTEXT_NO_ERROR})

(def window-hint-allowed-values
  {GLFW/GLFW_CLIENT_API
   {:opengl GLFW/GLFW_OPENGL_API
    :opengl-es GLFW/GLFW_OPENGL_ES_API
    :opengles GLFW/GLFW_OPENGL_ES_API
    :no GLFW/GLFW_NO_API
    :none GLFW/GLFW_NO_API
    nil GLFW/GLFW_NO_API}
   
   GLFW/GLFW_CONTEXT_CREATION_API
   {:native GLFW/GLFW_NATIVE_CONTEXT_API
    :egl GLFW/GLFW_EGL_CONTEXT_API
    :osmesa GLFW/GLFW_OSMESA_CONTEXT_API}
   
   GLFW/GLFW_OPENGL_PROFILE
   {:core GLFW/GLFW_OPENGL_CORE_PROFILE
    :compat GLFW/GLFW_OPENGL_COMPAT_PROFILE
    :any GLFW/GLFW_OPENGL_ANY_PROFILE}
   
   GLFW/GLFW_CONTEXT_ROBUSTNESS
   {:no-reset-notification GLFW/GLFW_NO_RESET_NOTIFICATION
    :lose-context-on-reset GLFW/GLFW_LOSE_CONTEXT_ON_RESET
    :no-robustness GLFW/GLFW_NO_ROBUSTNESS}
   
   GLFW/GLFW_CONTEXT_RELEASE_BEHAVIOR
   {:any GLFW/GLFW_ANY_RELEASE_BEHAVIOR
    :flush GLFW/GLFW_RELEASE_BEHAVIOR_FLUSH
    :none GLFW/GLFW_RELEASE_BEHAVIOR_NONE
    nil GLFW/GLFW_RELEASE_BEHAVIOR_NONE}})

(def window-attributes
  {:focused GLFW/GLFW_FOCUSED
   :iconified GLFW/GLFW_ICONIFIED
   :maximized GLFW/GLFW_MAXIMIZED
   :hovered GLFW/GLFW_HOVERED
   :visible GLFW/GLFW_VISIBLE
   :resizable GLFW/GLFW_RESIZABLE
   :decorated GLFW/GLFW_DECORATED
   :auto-iconify GLFW/GLFW_AUTO_ICONIFY
   :floating GLFW/GLFW_FLOATING
   :transparent-framebuffer GLFW/GLFW_TRANSPARENT_FRAMEBUFFER
   :focus-on-show GLFW/GLFW_FOCUS_ON_SHOW

   :client-api GLFW/GLFW_CLIENT_API
   :context-creation-api GLFW/GLFW_CONTEXT_CREATION_API
   :context-version-major GLFW/GLFW_CONTEXT_VERSION_MAJOR
   :context-version-minor GLFW/GLFW_CONTEXT_VERSION_MINOR
   :context-revision GLFW/GLFW_CONTEXT_REVISION
   :opengl-forward-compat GLFW/GLFW_OPENGL_FORWARD_COMPAT
   :opengl-debug-context GLFW/GLFW_OPENGL_DEBUG_CONTEXT
   :opengl-profile GLFW/GLFW_OPENGL_PROFILE
   :context-release-behavior GLFW/GLFW_CONTEXT_RELEASE_BEHAVIOR
   :context-no-error GLFW/GLFW_CONTEXT_NO_ERROR
   :context-robustness GLFW/GLFW_CONTEXT_ROBUSTNESS})

(defn default-window-hints
  []
  (GLFW/glfwDefaultWindowHints))

(defn window-hint
  [k v]
  (cond (keyword? k)
        (let [k (window-hints k)]
          (when-not k
            (throw (ex-info "invalid window hint" {:key k})))
          (recur k v))

        (keyword? v)
        (let [allowed-values (window-hint-allowed-values k)
              v (get allowed-values v)]
          (when-not v
            (throw (ex-info "invalid window hint value" {:key k :value v})))
          (recur k v))

        (boolean? v)
        (recur k (if v GLFW/GLFW_TRUE GLFW/GLFW_FALSE))

        (string? v)
        (GLFW/glfwWindowHintString (int k) ^String v)

        :else
        (GLFW/glfwWindowHint k v)))

(defn create-window
  [opts]
  (let [{:keys [width height title
                monitor share window-hints]} opts]
    (doseq [[k v] window-hints]
      (window-hint k v))
    (fail-if-result=
     0
     (GLFW/glfwCreateWindow
      (int (or width 640))
      (int (or height 480))
      (str (or title "Window"))
      (long (or monitor 0))
      (long (or share 0))))))

(defn focus-window
  [window]
  (GLFW/glfwFocusWindow window))

(defn show-window
  [window]
  (GLFW/glfwShowWindow window))

(defn get-framebuffer-size
  [^long window]
  (let [width (BufferUtils/createIntBuffer 1)
        height (BufferUtils/createIntBuffer 2)]
    (GLFW/glfwGetFramebufferSize window width height)
    [(.get width) (.get height)]))

(defn get-key
  [window key]
  (GLFW/glfwGetKey window key))

(defn get-mouse-button
  [window button]
  (GLFW/glfwGetMouseButton window button))

(defn get-window-attrib
  [window attrib]
  (GLFW/glfwGetWindowAttrib window attrib))

(defn make-context-current
  [window]
  (GLFW/glfwMakeContextCurrent window))

(defn get-current-context
  []
  (GLFW/glfwGetCurrentContext))

(defn swap-buffers
  [window]
  (GLFW/glfwSwapBuffers window))

(defn swap-interval
  [interval]
  (GLFW/glfwSwapInterval interval))

(defn set-window-should-close
  [window value]
  (GLFW/glfwSetWindowShouldClose window value))

(defn window-should-close
  [window]
  (GLFW/glfwWindowShouldClose window))

(defn poll-events
  []
  (GLFW/glfwPollEvents))

(defn set-key-callback
  [window cbfun]
  (GLFW/glfwSetKeyCallback
   window
   (reify GLFWKeyCallbackI
     (invoke [this window key scancode action mods]
       (cbfun window key scancode action mods)))))

(defn set-framebuffer-size-callback
  [window cbfun]
  (GLFW/glfwSetFramebufferSizeCallback
   window
   (reify GLFWFramebufferSizeCallbackI
     (invoke [this window width height]
       (cbfun window width height)))))

(defn free-callbacks
  [window]
  (Callbacks/glfwFreeCallbacks window))

(defn destroy-window
  [window]
  (GLFW/glfwDestroyWindow window))

(defn get-key-name
  [key scancode]
  (GLFW/glfwGetKeyName key scancode))

(defn get-key-scancode
  [key]
  (GLFW/glfwGetKeyScancode key))

(defn get-time
  []
  (GLFW/glfwGetTime))

(defmacro with-window
  [window opts & body]
  `(let [win# (create-window ~opts)
         ~window win#
         result# (try
                   ~@body
                   (finally
                     (free-callbacks win#)
                     (destroy-window win#)))]
     result#))

(defmacro with-gl-window
  [window opts & body]
  `(with-window win#
     ~opts
     (make-context-current win#)
     (GL/createCapabilities)
     (let [~window win#]
       ~@body)))

(defmacro with-glfw
  [opts & body]
  `(do
     (doseq [[~'k ~'v] (:init-hints ~opts)]
       (GLFW/glfwInitHint ~'k ~'v))
     (fail-if-result= false (GLFW/glfwInit))
     (let [result# (try
                     ~@body
                     (finally
                       (GLFW/glfwTerminate)))]
       result#)))
