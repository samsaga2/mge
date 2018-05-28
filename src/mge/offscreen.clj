(ns mge.offscreen
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]))

(defasmvar offscreen (* 32 24))

(defasmbyte dirty)

(defasmproc init-offscreen {:page :code}
  [:xor :a]
  [:ld [dirty] :a]

  [:ld :de offscreen]
  [:ld :hl offscreen]
  [:ld [:hl] :a]
  [:inc :hl]
  [:ex :de :hl]
  [:ld :bc (* 32 24)]
  [:ldir]

  [:ret])

(defasmproc update-offscreen {:page :code}
  [:ld :a [dirty]]
  [:or :a]
  [:ret :z]
  [:xor :a]
  [:ld [dirty] :a]

  ;; draw offscreen
  [:di]
  [:ld :hl offscreen]
  [:ld :de 0x1800]
  [:ld :bc (* 32 24)]
  [:call bios/LDIRVM]
  [:ei]
  [:ret])
