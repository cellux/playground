(ns omkamra.entfalter.plugins.pkg
  (:require [omkamra.entfalter.facts.os-release :as os-release]
            [omkamra.entfalter.helpers :as helpers]
            [omkamra.pygen.core :as py]))

(def PackageSpec
  [:map
   [:name string?]
   [:version {:optional true} string?]])

(def PkgPluginConfig
  [:map
   [:packages [:vector PackageSpec]]])

(py/define (select-package-manager facts)
  (assign! os-release (or (facts.get "os_release") {}))
  (assign! os-id (or (os-release.get "ID") ""))
  (assign! os-id (os-id.strip "\"'"))
  (assign! os-id (os-id.lower))
  (assign! id-like (or (os-release.get "ID_LIKE") ""))
  (assign! id-like (id-like.strip "\"'"))
  (assign! id-like (id-like.lower))
  (assign! id-like-parts (id-like.split))
  (if (or (in os-id ["debian" "ubuntu"])
          (in "debian" id-like-parts))
    (return "apt"))
  (raise (RuntimeError
          (str "unsupported package manager for os id="
               os-id
               ", id_like="
               id-like))))

(py/define (format-apt-package-spec package-spec)
  (assign! name (py-at package-spec "name"))
  (assign! version (package-spec.get "version"))
  (if version
    (return (str name "=" version)))
  (return name))

(py/define (install-packages-with-apt package-specs)
  (if (= (len package-specs) 0)
    (return nil))
  (assign! argv ["apt-get" "install" "-y"])
  (for package-spec package-specs
    (argv.append (::format-apt-package-spec package-spec)))
  (::helpers/run-command-checked argv))

(py/define (apply-pkg-plugin plugin-config facts)
  (assign! manager (::select-package-manager facts))
  (assign! package-specs (or (plugin-config.get "packages") []))
  (if (= manager "apt")
    [(::install-packages-with-apt package-specs)
     (::helpers/emit-event "pkg" "ok" "packages ensured via apt")]
    (raise (RuntimeError
            (str "unsupported package manager: " manager)))))

(def plugin
  {:fact-dependencies #{::os-release/fact}
   :config-schema PkgPluginConfig
   :apply-ref ::apply-pkg-plugin})
