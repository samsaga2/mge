(ns mge.engine-hscroll
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.engine-offscreen :as off]
            [mge.engine-tilemap :refer :all]
            [mge.engine-math :as math]
            [mge.engine-vdp :as vdp]))


(defasmword tilemap-width)
(defasmword tilemap-height)
(defasmword tilemap-map)
(defasmbyte page-lines)
(defasmbyte page-types)


(defasmproc load-attrs {:page :code}
  ;; ix=addr
  ;; width
  [:ld :l [:ix 0]]
  [:ld :h [:ix 1]]
  [:ld [tilemap-width] :hl]
  ;; height
  [:ld :l [:ix 2]]
  [:ld :h [:ix 3]]
  [:ld [tilemap-height] :hl]
  [:ret])


;; horizontal scroll

(defasmproc draw-horizontal-map {:page :code}
  ;; a=lines-page b=map-page iy=map-addr
  [:ld :a 1]
  [:ld [off/dirty] :a]

  ;; save map addr
  [:ld :hl tilemap-map]
  [:ld :a [:hl]]
  [:ld :iyl :a]
  [:inc :hl]
  [:ld :a [:hl]]
  [:ld :iyh :a]

  ;; write horizontal lines
  [:ld :hl off/offscreen]
  [:ld :b 32]
  (label :x
         [:push :hl]
         [:push :bc]

         ;; get line addr
         (set-konami5-page 3 [page-map])
         [:ld :e [:iy 0]]
         [:ld :d [:iy 1]]
         [:inc :iy]
         [:inc :iy]

         ;; write vertical line
         (set-konami5-page 3 [page-lines])
         [:ld :a [tilemap-height]]
         [:ld :b :a]
         (label :y
                [:ld :a [:de]]
                [:inc :de]
                [:ld [:hl] :a]

                [:push :de]
                [:ld :de 32]
                [:add :hl :de]
                [:pop :de]
                [:djnz :y])

         [:pop :bc]
         [:pop :hl]
         [:inc :hl]
         [:djnz :x])
  [:ret])

(defasmproc load-horizontal-map {:page :code}
  ;; a=lines-page b=map-page c=types-page ix=map-addr hl=types-addr

  ;; save pages
  [:ld [page-lines] :a]
  [:ld :a :b]
  [:ld [page-map] :a]
  [:ld :a :c]
  [:ld [page-types] :a]

  ;; save types addr
  [:ld [tilemap-types] :hl]

  ;; save map addr
  [:ld :a :ixl]
  [:ld :l :a]
  [:ld :a :ixh]
  [:ld :h :a]
  [:ld [tilemap-map] :hl]

  [:jp draw-horizontal-map])

(defasmproc scroll-left {:page :code}
  [:ld :hl tilemap-map]
  [:ld :e [:hl]]
  [:inc :hl]
  [:ld :d [:hl]]
  [:dec :de]
  [:dec :de]
  [:ld [:hl] :d]
  [:dec :hl]
  [:ld [:hl] :e]
  [:jp draw-horizontal-map])

(defasmproc scroll-right {:page :code}
  [:ld :hl tilemap-map]
  [:ld :e [:hl]]
  [:inc :hl]
  [:ld :d [:hl]]
  [:inc :de]
  [:inc :de]
  [:ld [:hl] :d]
  [:dec :hl]
  [:ld [:hl] :e]
  [:jp draw-horizontal-map])
