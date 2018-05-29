(ns mge.title
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.vdp :as vdp]))

(defasmproc load-patterns {:page :code}
  ;; HL=patterns
  [:push :hl]
  [:ld :hl 0]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc load-colors {:page :code}
  ;; HL=colors
  [:push :hl]
  [:ld :hl 0x2000]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc load-title {:page :code}
  ;; in a=page-pattern b=colors-pattern hl=patterns de=color
  [:push :de]
  [:push :bc]
  [:push :hl]
  [:push :af]

  [:call bios/DISSCR]
  [:di]

  ;; load patterns
  [:pop :af]
  (set-konami5-page 3 :a)
  [:call vdp/clear-name-table]
  [:pop :hl]
  [:call load-patterns]

  ;; load colors
  [:pop :af]
  (set-konami5-page 3 :a)
  [:pop :hl]
  [:call load-colors]

  [:ei]
  [:jp bios/ENASCR])
