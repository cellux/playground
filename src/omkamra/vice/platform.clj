(ns omkamra.vice.platform
  "C64 platform facts that the generic assembler must not assume.

  The assembler and its 6502 target are memory-map agnostic: nothing there
  knows where a C64 program should be placed or that the BASIC `SYS` harness
  page must stay clear. Those facts live here, and VICE links programs through
  `link`, which supplies them as ordinary link options.

  Pass `:regions` or `:reserved` to override a default deliberately."
  (:require
   [omkamra.assembler :as assembler]))

(def default-regions
  "First-fit placement regions used when an object has no `%region`.

  The BASIC `SYS` page is omitted; an explicit region or `%org` can still
  request it deliberately."
  [{:start 0xc000 :end 0xd000}
   {:start 0x0900 :end 0xa000}
   {:start 0xa000 :end 0xc000}
   {:start 0xe000 :end 0x10000}])

(def reserved-regions
  "Regions automatic placement keeps clear unless `:reserved` overrides them.

  `$0800-$08ff` is the BASIC `SYS` harness page a self-relocating PRG builds
  at `$0801`: the `10 SYS ...` line at `$0801` and the relocation stub at
  `$080d`. Explicit `%region`/`%org` placement may request it deliberately."
  [{:start 0x0800 :end 0x0900}])

(def link-defaults
  "The C64 placement options VICE passes to the generic linker."
  {:regions default-regions
   :reserved reserved-regions})

(defn link
  "Link `target` with the C64 placement defaults.

  `target` may be a deferred assembly descriptor, an assembled object graph, a
  relocatable object, or an already linked artifact. A descriptor is assembled
  with `assembler/build`. Options override `link-defaults`."
  ([target]
   (link target {}))
  ([target options]
   (let [options (merge link-defaults options)]
     (if (assembler/descriptor? target)
       (assembler/build target options)
       (assembler/link target options)))))
