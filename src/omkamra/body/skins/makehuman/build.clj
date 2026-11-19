(ns omkamra.body.skins.makehuman.build
  "Build the MakeHuman GLB skin from MakeHuman's CC0 assets.

  MakeHuman releases its base mesh, default skeleton and weights under CC0 1.0
  (see `resources/omkamra/body/skins/MAKEHUMAN.md`), and the community
  `Light Skin Female Bronze` diffuse map used for the skin is CC0 as well. This
  namespace converts those sources into the single skinned GLB that
  `omkamra.body.skins.makehuman` loads in the browser — without Blender:

  - the skeleton's joints are defined as sets of mesh vertices, so a joint's
    rest position is the mean of those vertices;
  - the weights file is JSON holding `[vertex-index weight]` pairs per bone.

  The converter also applies the library's conventions (D2): decimetres become
  metres, the body is moved so its feet rest on `y = 0`, and the result keeps
  Y up, facing +Z with the body's left on +X.

  The converter downloads the CC0 sources on demand, so the namespace is
  normally used through its cached references rather than a one-shot build.
  The artifacts are persisted below `.cache/refs`, not committed. Materialize
  them in the development JVM with:

      (require '[omkamra.body.skins.makehuman.build :as build])
      (alength @build/glb)
      (alength @build/texture)

  or warm the cache from a shell:

      clojure -M -m omkamra.body.skins.makehuman.build

  Use `omkamra.dev.cached-refs/refresh!` to force a rebuild."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [omkamra.body.math :as m]
            [omkamra.dev.cached-refs :as cached-refs])
  (:import [java.awt Color RenderingHints]
           [java.awt.image BufferedImage]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [javax.imageio IIOImage ImageIO ImageWriteParam]
           [javax.imageio.plugins.jpeg JPEGImageWriteParam]
           [javax.imageio.stream MemoryCacheImageOutputStream]))

(def base-url
  "https://raw.githubusercontent.com/makehumancommunity/makehuman/master/makehuman/data")

(def sources
  "CC0 sources downloaded on demand.

  The mesh, default skeleton and weights come from MakeHuman's repository;
  `diffuse.png` is the `Light Skin Female Bronze` diffuse map by MargaretToigo
  from the community asset site (node 1133)."
  {"base.obj" (str base-url "/3dobjs/base.obj")
   "default.mhskel" (str base-url "/rigs/default.mhskel")
   "default_weights.mhw" (str base-url "/rigs/default_weights.mhw")
   "diffuse.png"
   "http://files.makehumancommunity.org/sites/default/files/skins/1968/588659257/young_lightskinned_female_diffuse_Bronze.png"})

(def license "CC0-1.0")
(def attribution
  (str "MakeHuman base mesh, default skeleton and weights, "
       "(c) MakeHuman Team, CC0 1.0 Universal. "
       "https://www.makehumancommunity.org/"))

;; MakeHuman authors in decimetres; the library works in metres (D2).
(def scale 0.1)

;; ---------------------------------------------------------------------------
;; Sources

(defn- download
  "Download `name` from `sources` and return its body as a byte array."
  [name]
  (println "downloading" name)
  (with-open [in (io/input-stream (get sources name))
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn- load-json [name]
  (json/parse-string (slurp (download name) :encoding "UTF-8") true))

(defn- normalize-skeleton
  "Keywordize the references inside the parsed skeleton.

  `parse-string true` keywordizes map keys but not string values, so bone
  parents and joint references arrive as strings while bone and joint names
  are keywords."
  [skeleton]
  (update skeleton :bones
          (fn [bones]
            (into {}
                  (map (fn [[bone-name spec]]
                         [(keyword bone-name)
                          (-> spec
                              (update :parent #(some-> % keyword))
                              (update :head keyword)
                              (update :tail keyword))]))
                  bones))))

;; ---------------------------------------------------------------------------
;; Mesh

(def default-groups
  "OBJ groups kept in the generated skin.

  MakeHuman's base OBJ bundles the body with modelling helpers: a skirt, tights,
  hair, teeth, eyelashes, a tongue, genital and joint markers. The skin keeps the
  body and the eyeballs; the hair is generated separately (see `add-scalp`)."
  #{"body" "helper-l-eye" "helper-r-eye"})

(defn- parse-obj
  "Read positions, texture coordinates and faces from OBJ bytes.

  A face carries the OBJ group it belongs to; `keep-groups` filters them to
  `default-groups`."
  [source]
  (with-open [reader (io/reader source)]
    (reduce
     (fn [{:keys [positions uvs faces group] :as acc} line]
       (cond
         (str/starts-with? line "g ")
         (assoc acc :group (str/trim (subs line 2)))

         (str/starts-with? line "v ")
         (let [[_ x y z] (re-find #"^v\s+(\S+)\s+(\S+)\s+(\S+)" line)]
           (assoc acc :positions (conj positions [(Double/parseDouble x)
                                                  (Double/parseDouble y)
                                                  (Double/parseDouble z)])))

         (str/starts-with? line "vt ")
         (let [[_ u v] (re-find #"^vt\s+(\S+)\s+(\S+)" line)]
           (assoc acc :uvs (conj uvs [(Double/parseDouble u)
                                      (Double/parseDouble v)])))

         (str/starts-with? line "f ")
         (let [corners
               (mapv (fn [token]
                       (let [[position uv] (str/split token #"/" -1)]
                         [(dec (Long/parseLong position))
                          (if (str/blank? (or uv ""))
                            -1
                            (dec (Long/parseLong uv)))]))
                     (rest (str/split (str/trim line) #"\s+")))]
           (assoc acc :faces (conj faces [group corners])))

         :else acc))
     {:positions [] :uvs [] :faces [] :group nil}
     (line-seq reader))))

(defn- keep-groups
  "Filter parsed faces to `groups`, dropping everything else."
  [mesh groups]
  (update mesh :faces
          (fn [faces]
            (into [] (filter (fn [[group _corners]]
                               (contains? groups group)))
                  faces))))

(defn- triangulate
  "Split positions by texture coordinate and fan-triangulate the quads.

  Returns new positions (in metres), UVs, triangles, and the source position
  index of every new vertex so skin weights can be looked up."
  [{:keys [positions uvs faces]}]
  (let [vertices (volatile! [])
        uvs* (volatile! [])
        sources (volatile! [])
        key->index (volatile! {})
        triangles (volatile! [])
        index-of (fn [[position uv]]
                   (let [key [position uv]]
                     (or (get @key->index key)
                         (let [index (count @vertices)
                               [x y z] (nth positions position)]
                           (vswap! vertices conj [(* scale x) (* scale y) (* scale z)])
                           (vswap! uvs* conj (if (neg? uv)
                                               [0.0 0.0]
                                               (nth uvs uv)))
                           (vswap! sources conj position)
                           (vswap! key->index assoc key index)
                           index))))]
    (doseq [[_group corners] faces]
      (let [indices (mapv index-of corners)]
        (doseq [offset (range 1 (dec (count indices)))]
          (vswap! triangles conj [(nth indices 0)
                                  (nth indices offset)
                                  (nth indices (inc offset))]))))
    {:positions @vertices
     :uvs @uvs*
     :triangles @triangles
     :sources @sources}))

(defn- compute-normals [positions triangles]
  (let [zeroes (vec (repeat (count positions) [0.0 0.0 0.0]))
        accumulated
        (reduce
         (fn [normals [a b c]]
           (let [[ax ay az] (nth positions a)
                 [bx by bz] (nth positions b)
                 [cx cy cz] (nth positions c)
                 ux (- bx ax) uy (- by ay) uz (- bz az)
                 vx (- cx ax) vy (- cy ay) vz (- cz az)
                 nx (- (* uy vz) (* uz vy))
                 ny (- (* uz vx) (* ux vz))
                 nz (- (* ux vy) (* uy vx))]
             (-> normals
                 (update a (fn [[x y z]] [(+ x nx) (+ y ny) (+ z nz)]))
                 (update b (fn [[x y z]] [(+ x nx) (+ y ny) (+ z nz)]))
                 (update c (fn [[x y z]] [(+ x nx) (+ y ny) (+ z nz)])))))
         zeroes
         triangles)]
    (mapv (fn [[x y z]]
            (let [length (Math/sqrt (+ (* x x) (* y y) (* z z)))]
              (if (< length 1.0e-12)
                [0.0 1.0 0.0]
                [(/ x length) (/ y length) (/ z length)])))
          accumulated)))

;; ---------------------------------------------------------------------------
;; Skeleton

(defn- joint-positions
  "Rest position of every skeleton joint: the mean of its vertices, in metres,
  shifted so the feet rest on y = 0."
  [skeleton positions offset-y]
  (into {}
        (keep (fn [[joint-name indices]]
                (when (seq indices)
                  (let [n (double (count indices))
                        [x y z]
                        (reduce (fn [[sx sy sz] index]
                                  (let [[px py pz] (nth positions index)]
                                    [(+ sx px) (+ sy py) (+ sz pz)]))
                                [0.0 0.0 0.0]
                                indices)]
                    [joint-name [(* scale (/ x n))
                                 (+ (* scale (/ y n)) offset-y)
                                 (* scale (/ z n))]]))))
        (:joints skeleton)))

(defn- bone-rotation
  "Rest rotation of a bone: Y along head->tail, with a stable roll."
  [head tail]
  (let [direction (m/v-unit (m/v- tail head))
        reference (if (> (m/abs (m/v-dot [0.0 0.0 1.0] direction)) 0.95)
                    [1.0 0.0 0.0]
                    [0.0 0.0 1.0])
        x-axis (m/v-unit (m/v-cross reference direction))
        z-axis (m/v-cross x-axis direction)
        columns [(m/v3 (nth x-axis 0) (nth direction 0) (nth z-axis 0))
                 (m/v3 (nth x-axis 1) (nth direction 1) (nth z-axis 1))
                 (m/v3 (nth x-axis 2) (nth direction 2) (nth z-axis 2))]
        [[r00 r10 r20] [r01 r11 r21] [r02 r12 r22]] columns
        trace (+ r00 r11 r22)]
    (cond
      (pos? trace)
      (let [s (* 2.0 (Math/sqrt (inc trace)))]
        (m/q-unit [(/ (- r21 r12) s) (/ (- r02 r20) s) (/ (- r10 r01) s) (* 0.25 s)]))

      (and (> r00 r11) (> r00 r22))
      (let [s (* 2.0 (Math/sqrt (- (+ 1.0 r00) r11 r22)))]
        (m/q-unit [(* 0.25 s) (/ (+ r01 r10) s) (/ (+ r02 r20) s) (/ (- r21 r12) s)]))

      (> r11 r22)
      (let [s (* 2.0 (Math/sqrt (- (+ 1.0 r11) r00 r22)))]
        (m/q-unit [(/ (+ r01 r10) s) (* 0.25 s) (/ (+ r12 r21) s) (/ (- r02 r20) s)]))

      :else
      (let [s (* 2.0 (Math/sqrt (- (+ 1.0 r22) r00 r11)))]
        (m/q-unit [(/ (+ r02 r20) s) (/ (+ r12 r21) s) (* 0.25 s) (/ (- r10 r01) s)])))))

(defn- build-bones
  "Rest head, world rotation, local rotation and local translation per bone.

  Returns `[order state]`, bones parents first."
  [skeleton joints]
  (let [bones (:bones skeleton)
        state (volatile! {})
        order (volatile! [])
        visit
        (fn visit [name stack]
          (when-not (contains? @state name)
            (when (some #{name} stack)
              (throw (ex-info "cycle in MakeHuman skeleton" {:bone name})))
            (let [{:keys [parent]} (get bones name)
                  [parent-head parent-rotation]
                  (if parent
                    (do (visit parent (conj stack name))
                        (let [parent-state (get @state parent)]
                          [(:head parent-state) (:rotation parent-state)]))
                    [[0.0 0.0 0.0] m/quat-identity])
                  {:keys [head tail]} (get bones name)
                  head (get joints head)
                  tail (get joints tail)
                  world-rotation (bone-rotation head tail)
                  local-rotation (m/q-mul (m/q-conj parent-rotation) world-rotation)
                  local-translation (m/q-rotate (m/q-conj parent-rotation)
                                                (m/v- head parent-head))]
              (vswap! state assoc name {:head head
                                        :rotation world-rotation
                                        :local-rotation local-rotation
                                        :parent parent
                                        :translation local-translation})
              (vswap! order conj name))))]
    (doseq [name (keys bones)]
      (visit name []))
    [@order @state]))

(defn- inverse-bind-matrix
  "Column-major inverse of a bone's rest world matrix (rotation and head)."
  [head rotation]
  (let [[x y z w] rotation
        r00 (- 1.0 (* 2.0 (+ (* y y) (* z z))))
        r01 (* 2.0 (- (* x y) (* z w)))
        r02 (* 2.0 (+ (* x z) (* y w)))
        r10 (* 2.0 (+ (* x y) (* z w)))
        r11 (- 1.0 (* 2.0 (+ (* x x) (* z z))))
        r12 (* 2.0 (- (* y z) (* x w)))
        r20 (* 2.0 (- (* x z) (* y w)))
        r21 (* 2.0 (+ (* y z) (* x w)))
        r22 (- 1.0 (* 2.0 (+ (* x x) (* y y))))
        ;; Transpose of the rotation, then -R^T * head.
        [t00 t01 t02] [r00 r10 r20]
        [t10 t11 t12] [r01 r11 r21]
        [t20 t21 t22] [r02 r12 r22]
        [hx hy hz] head
        tx (- (+ (* t00 hx) (* t01 hy) (* t02 hz)))
        ty (- (+ (* t10 hx) (* t11 hy) (* t12 hz)))
        tz (- (+ (* t20 hx) (* t21 hy) (* t22 hz)))]
    [t00 t10 t20 0.0
     t01 t11 t21 0.0
     t02 t12 t22 0.0
     tx ty tz 1.0]))

;; ---------------------------------------------------------------------------
;; Weights

(defn- vertex-influences
  "Four strongest normalized `[bone-name weight]` pairs per glTF vertex."
  [weights sources]
  (let [per-position
        (reduce (fn [index [bone entries]]
                  (reduce (fn [index [position weight]]
                            (if (pos? weight)
                              (update-in index [position bone] (fnil + 0.0) weight)
                              index))
                          index
                          entries))
                {}
                weights)]
    (mapv (fn [position]
            (let [entries (->> (get per-position position {})
                               (sort-by val >)
                               (take 4))
                  total (reduce + 0.0 (map val entries))
                  total (if (pos? total) total 1.0)]
              (mapv (fn [[bone weight]] [(name bone) (/ weight total)]) entries)))
          sources)))

;; ---------------------------------------------------------------------------
;; Textures

(defn- load-texture
  "Read a MakeHuman skin texture and prepare it for embedding.

  The packs ship 2048x2048 PNG diffuse maps of several megabytes. glTF stores
  images verbatim, so the map is resampled to `size` pixels and encoded as a
  JPEG on a white background, which keeps the GLB small while staying sharp at
  body scale."
  [image-bytes size]
  (let [image (ImageIO/read (ByteArrayInputStream. image-bytes))
        width (.getWidth image)
        height (.getHeight image)
        scale (min 1.0 (/ (double size) (max width height)))
        target-width (max 1 (int (Math/round (* scale width))))
        target-height (max 1 (int (Math/round (* scale height))))
        scaled (BufferedImage. target-width target-height BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics scaled)]
    (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION
                       RenderingHints/VALUE_INTERPOLATION_BICUBIC)
    (.setColor graphics Color/WHITE)
    (.fillRect graphics 0 0 target-width target-height)
    (.drawImage graphics image 0 0 target-width target-height nil)
    (.dispose graphics)
    (let [buffer (ByteArrayOutputStream.)
          writer (ImageIO/getImageWritersByFormatName "jpeg")]
      (when-not (.hasNext writer)
        (throw (ex-info "No JPEG writer available" {})))
      (let [writer (.next writer)
            params (JPEGImageWriteParam. nil)]
        (.setCompressionMode params ImageWriteParam/MODE_EXPLICIT)
        (.setCompressionQuality params 0.92)
        (with-open [out (MemoryCacheImageOutputStream. buffer)]
          (.setOutput writer out)
          ;; The JPEG plugin only implements the three-argument write, whose
          ;; first argument lets the caller replace the stream metadata.
          (.write writer nil ^IIOImage (IIOImage. scaled nil nil) ^ImageWriteParam params)
          (.dispose writer)))
      {:bytes (.toByteArray buffer)
       :mime-type "image/jpeg"
       :width target-width
       :height target-height})))

;; ---------------------------------------------------------------------------
;; glTF writing

(defn- f32-array [values]
  (let [buffer (ByteBuffer/allocate (* 4 (count values)))]
    ;; glTF binary data is little-endian; java.nio defaults to big-endian.
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (let [floats (.asFloatBuffer buffer)]
      (doseq [value values] (.put floats (float value))))
    (.array buffer)))

(defn- u16-array [values]
  (let [buffer (ByteBuffer/allocate (* 2 (count values)))]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (doseq [value values] (.putShort buffer (short value)))
    (.array buffer)))

(defn- le-u32 [value]
  (let [buffer (ByteBuffer/allocate 4)]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.putInt buffer (int value))
    (.array buffer)))

(defn- concat-bytes [chunks]
  (let [out (ByteArrayOutputStream.)]
    (doseq [chunk chunks] (.write out ^bytes chunk))
    (.toByteArray out)))

(defn- add-buffer-view!
  "Append `payload` to `buffer`, aligned to 4 bytes. Returns the view index."
  [^ByteArrayOutputStream buffer views ^bytes payload target]
  (let [size (alength (.toByteArray buffer))
        padding (mod (- 4 (mod size 4)) 4)]
    (when (pos? padding)
      (.write buffer (byte-array padding))))
  (let [offset (alength (.toByteArray buffer))]
    (.write buffer payload)
    (let [padding (mod (- 4 (mod (alength payload) 4)) 4)]
      (when (pos? padding)
        (.write buffer (byte-array padding))))
    (vswap! views conj (cond-> {:buffer 0
                                :byteOffset offset
                                :byteLength (alength payload)}
                         target (assoc :target target)))
    (dec (count @views))))

(defn- make-accessor [buffer-view component-type count kind minimum maximum]
  (cond-> {:bufferView buffer-view
           :componentType component-type
           :count count
           :type kind}
    minimum (assoc :min minimum)
    maximum (assoc :max maximum)))

(defn- padded-influences [entries]
  (into (vec entries) (repeat (- 4 (count entries)) ["" 0.0])))

(defn- build-glb
  [{:keys [positions normals uvs triangles influences bone-order bone-state texture]}]
  (let [buffer (ByteArrayOutputStream.)
        views (volatile! [])
        accessors (volatile! [])
        add-accessor! (fn [value] (vswap! accessors conj value) (dec (count @accessors)))
        bone-index (into {} (map-indexed (fn [index bone] [(name bone) index]) bone-order))
        minimum (mapv (fn [axis] (reduce #(min %1 (nth %2 axis)) ##Inf positions)) (range 3))
        maximum (mapv (fn [axis] (reduce #(max %1 (nth %2 axis)) ##-Inf positions)) (range 3))
        position-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (f32-array (mapcat identity positions))
                                                        34962)
                                      5126 (count positions) "VEC3" minimum maximum))
        normal-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (f32-array (mapcat identity normals))
                                                        34962)
                                      5126 (count normals) "VEC3" nil nil))
        ;; glTF's texture origin is the top-left corner, OBJ's is the
        ;; bottom-left, so V is flipped when the mesh is written.
        uvs (mapv (fn [[u v]] [u (- 1.0 v)]) uvs)
        uv-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (f32-array (mapcat identity uvs))
                                                        34962)
                                      5126 (count uvs) "VEC2" nil nil))
        joint-indices (mapcat (fn [entries]
                                (map (fn [[bone _]] (get bone-index bone 0))
                                     (padded-influences entries)))
                              influences)
        weight-values (mapcat (fn [entries]
                                (map second (padded-influences entries)))
                              influences)
        joints-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (u16-array joint-indices)
                                                        34962)
                                      5123 (count influences) "VEC4" nil nil))
        weights-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (f32-array weight-values)
                                                        34962)
                                      5126 (count influences) "VEC4" nil nil))
        index-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (u16-array (mapcat identity triangles))
                                                        34963)
                                      5123 (* 3 (count triangles)) "SCALAR" nil nil))
        ibm (mapcat (fn [name]
                      (inverse-bind-matrix (:head (get bone-state name))
                                           (:rotation (get bone-state name))))
                    bone-order)
        ibm-accessor
        (add-accessor! (make-accessor (add-buffer-view! buffer views
                                                        (f32-array ibm) nil)
                                      5126 (count bone-order) "MAT4" nil nil))
        texture-index (when texture
                        (add-buffer-view! buffer views (:bytes texture) nil))
        node-index (into {} (map-indexed (fn [index name] [name index]) bone-order))
        nodes (mapv (fn [bone]
                      (let [{:keys [translation local-rotation]} (get bone-state bone)]
                        {:name (name bone)
                         :translation (mapv #(Double/parseDouble (format "%.6f" %)) translation)
                         :rotation (mapv #(Double/parseDouble (format "%.8f" %)) local-rotation)}))
                    bone-order)
        nodes (reduce (fn [nodes name]
                        (if-let [parent (:parent (get bone-state name))]
                          (update-in nodes [(node-index parent) :children]
                                     (fnil conj []) (node-index name))
                          nodes))
                      nodes
                      bone-order)
        root-nodes (mapv node-index (filter (fn [bone]
                                              (nil? (:parent (get bone-state bone))))
                                            bone-order))
        mesh-index (count nodes)
        nodes (conj nodes {:name "body" :mesh 0 :skin 0})
        gltf {:asset {:version "2.0"
                      :generator "omkamra.body omkamra.body.skins.makehuman.build"
                      :extras {:license license :attribution attribution}}
              :scene 0
              :scenes [{:nodes (conj root-nodes mesh-index)}]
              :nodes nodes
              :meshes [{:name "makehuman"
                        :primitives [{:attributes {:POSITION position-accessor
                                                   :NORMAL normal-accessor
                                                   :TEXCOORD_0 uv-accessor
                                                   :JOINTS_0 joints-accessor
                                                   :WEIGHTS_0 weights-accessor}
                                      :indices index-accessor
                                      :material 0
                                      :mode 4}]}]
              :skins [{:name "makehuman-default"
                       :joints (mapv node-index bone-order)
                       :inverseBindMatrices ibm-accessor
                       :skeleton (first root-nodes)}]
              :materials [{:name "skin"
                           :pbrMetallicRoughness
                           (cond-> {:baseColorFactor (if texture
                                                       [1.0 1.0 1.0 1.0]
                                                       [0.82 0.75 0.68 1.0])
                                    :metallicFactor 0.0
                                    :roughnessFactor 0.78}
                             texture (assoc :baseColorTexture {:index 0 :texCoord 0}))
                           :doubleSided false}]
              :images (when texture [{:bufferView texture-index
                                      :mimeType (:mime-type texture)}])
              :samplers (when texture [{:magFilter 9729 :minFilter 9987
                                        :wrapS 10497 :wrapT 10497}])
              :textures (when texture [{:sampler 0 :source 0}])
              :buffers [{:byteLength 0}]
              :bufferViews @views
              :accessors @accessors}
        gltf (into {} (remove (comp nil? val) gltf))
        bin-bytes (.toByteArray buffer)
        bin-bytes (let [padding (mod (- 4 (mod (alength bin-bytes) 4)) 4)]
                    (concat-bytes [bin-bytes (byte-array padding)]))
        gltf (assoc-in gltf [:buffers 0 :byteLength] (alength bin-bytes))
        json-bytes (let [bytes (.getBytes (json/generate-string gltf) "UTF-8")
                         padding (mod (- 4 (mod (alength bytes) 4)) 4)]
                     ;; glTF pads the JSON chunk with spaces, the BIN chunk
                     ;; with zero bytes.
                     (concat-bytes [bytes (byte-array padding (byte 32))]))
        total (+ 12 8 (alength json-bytes) 8 (alength bin-bytes))]
    {:gltf gltf
     :bytes (concat-bytes [(le-u32 0x46546C67)
                           (le-u32 2)
                           (le-u32 total)
                           (le-u32 (alength json-bytes))
                           (le-u32 0x4E4F534A)
                           json-bytes
                           (le-u32 (alength bin-bytes))
                           (le-u32 0x004E4942)
                           bin-bytes])}))

;; ---------------------------------------------------------------------------
;; Cached assets

(defn- prepare-texture
  "Download the CC0 diffuse map and prepare it for embedding."
  []
  (load-texture (download "diffuse.png") 1024))

(defn- assemble-glb
  "Assemble the GLB from the downloaded MakeHuman sources and a texture.

  `prepared-texture` is a prepared texture map (see `load-texture`), or nil for
  a plain skin colour. Returns `{:bytes :gltf :vertices :triangles :joints}`."
  [prepared-texture]
  (let [skeleton (normalize-skeleton (load-json "default.mhskel"))
        weights (:weights (load-json "default_weights.mhw"))
        source-mesh (keep-groups (parse-obj (download "base.obj")) default-groups)
        {:keys [positions uvs triangles] :as mesh} (triangulate source-mesh)
        normals (compute-normals positions triangles)
        ; MakeHuman's origin sits at hip height; move the feet to y = 0.
        lowest (reduce min (map second positions))
        offset-y (- lowest)
        positions (mapv (fn [[x y z]] [x (+ y offset-y) z]) positions)
        joints (joint-positions skeleton (:positions source-mesh) offset-y)
        [bone-order bone-state] (build-bones skeleton joints)
        influences (vertex-influences weights (:sources mesh))
        {:keys [gltf bytes]} (build-glb {:positions positions
                                         :normals normals
                                         :uvs uvs
                                         :triangles triangles
                                         :influences influences
                                         :bone-order bone-order
                                         :bone-state bone-state
                                         :texture prepared-texture})]
    {:gltf gltf
     :bytes bytes
     :vertices (count positions)
     :triangles (count triangles)
     :joints (count (:joints (first (:skins gltf))))}))

(def texture
  "Cached reference to the prepared CC0 diffuse map, as JPEG bytes.

  The first dereference downloads the source map, resamples it to 1024px and
  persists it below `.cache/refs/omkamra/body/skins/makehuman-texture`."
  (cached-refs/cached-ref
   :omkamra.body.skins/makehuman-texture
   #(:bytes (prepare-texture))))

(def glb
  "Cached reference to the generated MakeHuman GLB skin, as bytes.

  The first dereference builds it from the CC0 sources and the `texture`
  reference, and persists it below
  `.cache/refs/omkamra/body/skins/makehuman-glb`."
  (cached-refs/cached-ref
   :omkamra.body.skins/makehuman-glb
   #(:bytes (assemble-glb {:bytes @texture :mime-type "image/jpeg"}))))

(defn -main
  "Dereference the cached assets, producing and persisting them on first use."
  [& _]
  (prn {:texture (alength ^bytes @texture)
        :glb (alength ^bytes @glb)}))