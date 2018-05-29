(ns mge.offscreen
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [mge.math :as m]))


(defasmvar offscreen (* 32 24))

(defasmbyte dirty)


;; core

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

(defasmproc set-tile {:page :code}
  ;; in a=x b=y c=tile
  [:ld :l :b]
  [:ld :h 0]
  (m/mul-hl-by-pow2 32)

  [:ld :e :a]
  [:ld :d 0]
  [:add :hl :de]

  [:ld :de offscreen]
  [:add :hl :de]

  [:ld [:hl] :c]

  [:ld :a 1]
  [:ld [dirty] :a]
  [:ret])

(defasmproc write-print {:page :code}
  ;; a=x b=y de=str
  [:ld :l :b]
  [:ld :h 0]
  (m/mul-hl-by-pow2 32)

  [:ld :c :a]
  [:ld :b 0]
  [:add :hl :bc]

  [:ld :bc offscreen]
  [:add :hl :bc]

  [:ld :a 1]
  [:ld [dirty] :a]

  (label :loop
         [:ld :a [:de]]
         [:or :a]
         [:ret :z]
         [:ld [:hl] :a]
         [:inc :hl]
         [:inc :de]
         [:jp :loop]))
