(ns omkamra.mist64.hdl
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-antlr.core :as antlr]
   [clj-antlr.coerce :refer [tree->sexpr]])
  (:import
   [java.nio.file Path Files FileVisitor FileVisitResult]
   [org.antlr.v4.tool Grammar LexerGrammar]
   [org.antlr.v4.runtime CharStreams CommonTokenStream]))

(def mist64-sources
  #{"c64_mist.vhd"
    "sdram.v"
    "data_io.v"
    "user_io.v"
    "sigma_delta_dac.v"
    "cartridge.v"
    "progressbar.v"
    "c64_midi.vhd"
    "acia.v"
    "rom_reconfig_pal.vhd"
    "rom_reconfig_ntsc.vhd"
    "pll_c64_reconfig.vhd"
    "pll_c64.vhd"
    "pll.vhd"
    "reu.vhd"
    "fpga64_sid_iec.vhd"
    "mos6526.v"
    "sid8580.v"
    "sid_voice.v"
    "sid_envelope.v"
    "sid_filters.v"
    "fpga64_rgbcolor.vhd"
    "gen_ram.vhd"
    "fpga64_buslogic_roms_mmu.vhd"
    "rom_c64_chargen.vhd"
    "video_vicII_656x_e.vhd"
    "video_vicII_656x_a.vhd"
    "sid_top.vhd"
    "sid_regs.vhd"
    "sid_ctrl.vhd"
    "oscillator.vhd"
    "wave_map.vhd"
    "adsr_multi.vhd"
    "mult_acc.vhd"
    "my_math_pkg.vhd"
    "sid_filter.vhd"
    "Q_table.vhd"
    "sid_mixer.vhd"
    "cpu_6510.vhd"
    "T65.vhd"
    "T65_Pack.vhd"
    "T65_MCode.vhd"
    "T65_ALU.vhd"
    "io_ps2_keyboard.vhd"
    "fpga64_keyboard_matrix_mark_mcdougall.vhd"
    "c1541_sd.vhd"
    "mist_sd_card.sv"
    "c1541_logic.vhd"
    "gen_rom.vhd"
    "spram.vhd"
    "via6522.vhd"
    "gcr_floppy.vhd"
    "c1530.vhd"
    "tap_fifo.vhd"
    "mist_video.v"
    "scandoubler.v"
    "osd.v"
    "cofi.sv"
    "rgb2ypbpr.v"})

(defn resolve-sources
  [& paths]
  (let [sources (atom {})
        visitor (reify FileVisitor
                  (postVisitDirectory [this dir exc]
                    FileVisitResult/CONTINUE)
                  (preVisitDirectory [this dir attrs]
                    FileVisitResult/CONTINUE)
                  (visitFile [this path attrs]
                    (let [fn (str (.getFileName path))]
                      (when (mist64-sources fn)
                        (swap! sources assoc fn (.toFile path)))
                      FileVisitResult/CONTINUE))
                  (visitFileFailed [this file exc]
                    FileVisitResult/CONTINUE))]
    (doseq [p (->> paths (map io/file) (map #(.toPath %)))]
      (Files/walkFileTree p visitor))
    @sources))

(defn slurp-grammar
  [filename]
  (->> (str "omkamra/mist64/grammars/" filename)
       io/resource
       slurp))

(def parse-vhd (antlr/parser (slurp-grammar "vhdl.g4")
                             {:case-sensitive? false
                              :root "design_file"}))

(defn make-verilog-parser
  [lexer-grammar parser-grammar root-rule opts]
  (let [lg (LexerGrammar. (slurp-grammar lexer-grammar))
        g (Grammar. (slurp-grammar parser-grammar) lg)
        atn (.getATN g)]
    (fn [input]
      (let [engine (.createLexerInterpreter lg (CharStreams/fromString input))
            tokens (CommonTokenStream. engine)
            parser (.createParserInterpreter g tokens)
            tree (.parse parser (-> g (.getRule root-rule) .index))]
        (tree->sexpr {:tree tree
                      :parser parser
                      :grammar g
                      :opts opts})))))

(def parse-v (make-verilog-parser "VerilogLexer.g4"
                                  "VerilogParser.g4"
                                  "source_text"
                                  {}))

(def parse-sv (make-verilog-parser "SystemVerilogLexer.g4"
                                   "SystemVerilogParser.g4"
                                   "source_text"
                                   {}))

(defn parse-source
  [source-type source-code]
  (case source-type
    :vhd (parse-vhd source-code)
    :v (parse-v source-code)
    :sv (parse-sv source-code)))

(defn parse-file
  [^java.io.File file]
  (let [source-path (str file)
        source-type (cond (.endsWith source-path ".vhd") :vhd
                          (.endsWith source-path ".v") :v
                          (.endsWith source-path ".sv") :sv
                          :else (throw (ex-info "invalid file type" {:source-path source-path})))
        source-code (slurp file)]
    (parse-source source-type source-code)))

(defmulti parse
  (fn [state tree]
    (first tree)))

(defmethod parse :default
  [state tree]
  (first tree))

(defn parse-file*
  [state ^java.io.File input-file]
  (let [tree (parse-file input-file)]
    (parse state tree)))

(defn make-state
  []
  {})

(defn parse1
  [fn]
  (let [sources (resolve-sources "/home/rb/src/mist-devel")
        file (get sources fn)]
    (-> (make-state)
        (parse-file* file))))
