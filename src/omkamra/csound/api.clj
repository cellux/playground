(ns omkamra.csound.api
  (:require
   [omkamra.jnr.library :as library]))

;; flags for csoundInitialize
(def CSOUNDINIT_NO_SIGNAL_HANDLER 1)
(def CSOUNDINIT_NO_ATEXIT 2)

(library/define $csound "csound64"
  (^int csoundInitialize [^int flags])
  (^Pointer csoundCreate [^Pointer hostData])
  (^void csoundDestroy [^Pointer csound])

  (^int csoundGetVersion [])
  (^int csoundGetAPIVersion [])

  (^Pointer csoundParseOrc [^Pointer csound ^String str])
  (^int csoundCompileTree [^Pointer csound ^Pointer root])
  (^void csoundDeleteTree [^Pointer csound ^Pointer tree])
  (^int csoundCompileOrc [^Pointer csound ^String str])

  (^int csoundStart [^Pointer csound])

  (^int csoundCompileCsdText [^Pointer csound ^String csd_text])

  (^int csoundPerform [^Pointer csound])
  (^int csoundPerformKsmps [^Pointer csound])
  (^int csoundPerformBuffer [^Pointer csound])

  (^void csoundStop [^Pointer csound])
  (^int csoundCleanup [^Pointer csound])
  (^void csoundReset [^Pointer csound])

  (^int64_t csoundGetCurrentTimeSamples [^Pointer csound])

  (^int csoundGetSizeOfMYFLT [])

  (^void csoundSetOutput [^Pointer csound ^String name ^String type ^String format])
  (^void csoundSetInput [^Pointer csound ^String name])

  (^void csoundSetMIDIInput [^Pointer csound ^String name])
  (^void csoundSetMIDIFileInput [^Pointer csound ^String name])
  (^void csoundSetMIDIOutput [^Pointer csound ^String name])
  (^void csoundSetMIDIFileOutput [^Pointer csound ^String name])

  (^long csoundGetInputBufferSize [^Pointer csound])
  (^long csoundGetOutputBufferSize [^Pointer csound])

  (^Pointer csoundGetInputBuffer [^Pointer csound])
  (^Pointer csoundGetOutputBuffer [^Pointer csound])

  (^void csoundSetHostImplementedAudioIO [^Pointer csound ^int state ^int bufSize])

  (^int csoundReadScore [^Pointer csound ^String str])
  (^double csoundGetScoreTime [^Pointer csound])
  (^int csoundIsScorePending [^Pointer csound])
  (^void csoundSetScorePending [^Pointer csound ^int pending])

  (^void csoundCreateMessageBuffer [^Pointer csound ^int toStdOut])
  (^String csoundGetFirstMessage [^Pointer csound])
  (^int csoundGetFirstMessageAttr [^Pointer csound])
  (^void csoundPopFirstMessage [^Pointer csound])
  (^int csoundGetMessageCnt [^Pointer csound])
  (^void csoundDestroyMessageBuffer [^Pointer csound])

  (^int csoundTableLength [^Pointer csound ^int table])
  (^void csoundTableCopyOut [^Pointer csound ^int table ^Pointer dest])
  (^void csoundTableCopyIn [^Pointer csound ^int table ^Pointer src]))
