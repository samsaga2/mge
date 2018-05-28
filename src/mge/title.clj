(ns mge.title
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.vdp :as vdp]))

(defasmproc clear-name {:page :code}
  [:ld :hl 0x1800]
  [:call vdp/set-write-addr]
  [:ld :b 3]
  [:xor :a]
  (label :loop
         [:out [0x98] :a]
         [:inc :a]
         [:jp :nz :loop]
         [:djnz :loop])
  [:ret])

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

  [:pop :af]
  (set-konami5-page 3 :a)
  [:call clear-name]
  [:pop :hl]
  [:call load-patterns]

  [:pop :af]
  (set-konami5-page 3 :a)
  [:pop :hl]
  [:call load-colors]

  [:ei]
  [:jp bios/ENASCR])
