(ns rb.explores.dev.http)

(defn index
  "Example namespace-index endpoint for omkamra.dev.http."
  [_request]
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body "Hello from rb.explores.dev.http/index\n"})

(defn hello
  "Example endpoint for omkamra.dev.http."
  [_request]
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body "Hello from rb.explores.dev.http/hello\n"})
