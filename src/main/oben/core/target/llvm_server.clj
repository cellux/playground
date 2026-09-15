(ns oben.core.target.llvm-server
  "An Oben target backed by the local llvm-http-server byte-buffer ABI.

   Only scalar integer and floating-point function parameters and return values
   are currently supported.  The target emits an ABI adapter for each uploaded
   Oben function; it decodes a native-endian byte buffer, calls the function,
   and encodes its result into the output buffer expected by llvm-http-server."
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [oben.compiler :as compiler]
            [oben.core.context :as ctx]
            [oben.core.protocols.Target :as Target]
            [omkamra.llvm.ir :as ir]
            [omkamra.llvm.platform :as platform])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.util UUID]))

(def default-url "http://127.0.0.1:18080")

(def default-attrs
  {:address-size platform/address-size
   :align-min 1})

(defn- endpoint
  [url & segments]
  (str (str/replace url #"/+$" "") "/" (str/join "/" segments)))

(defn- request!
  [request]
  (let [{:keys [status body] :as response}
        (http/request (assoc request :throw-exceptions false))]
    (if (<= 200 status 299)
      response
      (throw (ex-info "llvm-server request failed"
                      {:status status :body body :url (:url request)})))))

(defn- scalar-layout
  "Returns the byte-buffer layout for a scalar LLVM IR type, or throws.
   The server and this target are local by design, so the byte layout is the
   native byte order of the process running this Clojure target."
  [ir-type]
  (cond
    (= ir-type :void) {:ir-type ir-type :llvm "void" :bytes 0 :kind :void}
    (= ir-type :float) {:ir-type ir-type :llvm "float" :bytes 4 :kind :f32}
    (= ir-type :double) {:ir-type ir-type :llvm "double" :bytes 8 :kind :f64}
    (and (vector? ir-type) (= :integer (first ir-type)))
    (let [bits (second ir-type)
          bytes (case bits 1 1 8 1 16 2 32 4 64 8 nil)]
      (when-not bytes
        (throw (ex-info "unsupported integer width for llvm-server target"
                        {:type ir-type})))
      {:ir-type ir-type :llvm (str "i" bits) :bytes bytes :kind :integer :bits bits})
    :else
    (throw (ex-info "llvm-server target supports scalar values only"
                    {:type ir-type}))))

(defn- put-value!
  [^ByteBuffer buffer {:keys [kind bits]} value]
  (case kind
    :integer (case bits
               (1 8) (.put buffer (byte value))
               16 (.putShort buffer (short value))
               32 (.putInt buffer (int value))
               64 (.putLong buffer (long value)))
    :f32 (.putFloat buffer (float value))
    :f64 (.putDouble buffer (double value))))

(defn- get-value!
  [^ByteBuffer buffer {:keys [kind bits]}]
  (case kind
    :void nil
    :integer (case bits
               (1 8) (.get buffer)
               16 (.getShort buffer)
               32 (.getInt buffer)
               64 (.getLong buffer))
    :f32 (.getFloat buffer)
    :f64 (.getDouble buffer)))

(defn- native-buffer
  [size]
  (doto (ByteBuffer/allocate size)
    (.order (ByteOrder/nativeOrder))))

(defn- serialize-args
  [layouts args]
  (when-not (= (count layouts) (count args))
    (throw (ex-info "invalid number of arguments" {:expected (count layouts)
                                                     :actual (count args)})))
  (let [buffer (native-buffer (reduce + (map :bytes layouts)))]
    (doseq [[layout arg] (map vector layouts args)]
      (put-value! buffer layout arg))
    (.array buffer)))

(defn- llvm-gep
  [name base offset]
  (str "  %" name " = getelementptr i8, ptr %" base ", i64 " offset))

(defn- adapter-source
  [adapter-name f]
  (let [params (:params f)
        param-layouts (mapv #(scalar-layout (:type %)) params)
        return-layout (scalar-layout (:result-type f))
        input-size (reduce + (map :bytes param-layouts))
        output-size (:bytes return-layout)
        offsets (butlast (reductions + 0 (map :bytes param-layouts)))
        decoded (mapv (fn [index layout offset]
                        [(llvm-gep (str "arg" index "-ptr") "input" offset)
                         (str "  %arg" index " = load " (:llvm layout)
                              ", ptr %arg" index "-ptr, align 1")])
                      (range) param-layouts offsets)
        call-args (str/join ", "
                           (map-indexed (fn [index layout]
                                          (str (:llvm layout) " %arg" index))
                                        param-layouts))
        call (str "  " (when-not (= :void (:kind return-layout)) "%result = ")
                  "call " (:llvm return-layout) " @" (:name f) "(" call-args ")")
        encode (when-not (= :void (:kind return-layout))
                 [(llvm-gep "result-ptr" "output" 0)
                  (str "  store " (:llvm return-layout)
                       " %result, ptr %result-ptr, align 1")])]
    (str/join "\n"
              (concat
               [(str "define i64 @" adapter-name
                     "(ptr %input, i64 %input_length, ptr %output, i64 %output_capacity) {")
                "entry:"
                (str "  %input-ok = icmp eq i64 %input_length, " input-size)
                "  br i1 %input-ok, label %decode, label %invalid-input"
                "invalid-input:"
                "  ret i64 -1"
                "decode:"]
               (mapcat identity decoded)
               [call
                (str "  %output-ok = icmp uge i64 %output_capacity, " output-size)
                "  br i1 %output-ok, label %encode, label %output-too-small"
                "output-too-small:"
                "  ret i64 -2"
                "encode:"]
               encode
               [(str "  ret i64 " output-size)
                "}"
                ""]))))

(defn- module-source
  [source adapter]
  (str source "\n" adapter))

(defn- load-module!
  [url namespace module source]
  (request! {:method :put
             :url (endpoint url namespace "modules" module)
             :content-type "text/plain"
             :body source}))

(defn- invoke!
  [url namespace {:keys [module adapter param-layouts return-layout]} args]
  (let [response (request! {:method :post
                            :url (str (endpoint url namespace "modules" module
                                                "symbols" adapter "invoke")
                                      "?output_capacity=" (:bytes return-layout))
                            :content-type "application/octet-stream"
                            :as :byte-array
                            :body (serialize-args param-layouts args)})
        return-code (some-> (get (:headers response) "x-llvm-return-code")
                             Long/parseLong)]
    (when-not (= return-code (long (:bytes return-layout)))
      (throw (ex-info "llvm-server function invocation failed"
                      {:return-code return-code :module module :adapter adapter})))
    (let [body ^bytes (:body response)]
      (when-not (= (alength body) (:bytes return-layout))
        (throw (ex-info "llvm-server returned an unexpected output length"
                        {:expected (:bytes return-layout) :actual (alength body)})))
      (get-value! (doto (ByteBuffer/wrap body) (.order (ByteOrder/nativeOrder)))
                  return-layout))))

(defrecord LLVMServerTarget [ctx attrs url namespace modules next-module-id]
  Target/protocol

  (compile-function [this fnode]
    (if (contains? modules fnode)
      this
      (let [{:keys [ctx source function]} (compiler/compile-function this ctx fnode)
            module-id (str "oben-" next-module-id)
            adapter-name (str "oben_entry_" next-module-id)
            param-layouts (mapv #(scalar-layout (:type %)) (:params function))
            return-layout (scalar-layout (:result-type function))
            adapter (adapter-source adapter-name function)
            source (compiler/verify-module-source!
                    (module-source source adapter))]
        (load-module! url namespace module-id source)
        (assoc this
               :ctx ctx
               :modules (assoc modules fnode {:module module-id
                                              :adapter adapter-name
                                              :param-layouts param-layouts
                                              :return-layout return-layout})
               :next-module-id (inc next-module-id)))))

  (invoke-function [_ fnode args]
    (if-let [compiled (get modules fnode)]
      (invoke! url namespace compiled args)
      (throw (ex-info "function was not compiled for llvm-server target"
                      {:function fnode}))))

  (dispose [this]
    (try
      (request! {:method :delete :url (endpoint url namespace)})
      (catch clojure.lang.ExceptionInfo e
        ;; The namespace may never have received a module, in which case the
        ;; server correctly reports it as absent. Disposal remains idempotent.
        (when-not (= 404 (:status (ex-data e)))
          (throw e))))
    this))

(defn create
  [{:keys [attrs url namespace target-layout] :as _opts}]
  (let [attrs (merge default-attrs attrs)]
    (map->LLVMServerTarget
     {:ctx (ctx/create {:target-attrs attrs :target-layout target-layout})
      :attrs attrs
      :url (or url default-url)
      :namespace (or namespace (str "oben-" (UUID/randomUUID)))
      :modules {}
      :next-module-id 1})))
