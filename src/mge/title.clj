(ns mge.title
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.util.graphics :refer [convert-screen2]]
            [clj-z80.msx.util.compress :refer [compress-lz77]]))

(defasmproc load-patterns {:page :code}
  ;; HL=patterns
  [:push :hl]
  [:ld :hl 0]
  [:call bios/SETWRT]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc load-colors {:page :code}
  ;; HL=colors
  [:push :hl]
  [:ld :hl 0x2000]
  [:call bios/SETWRT]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])
