(ns mge.tilemap
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.math :as math]))

(defasmword tilemap-width)
(defasmword tilemap-height)
(defasmword tilemap-map)
(defasmbyte page-lines)
(defasmbyte page-map)
(defasmvar offscreen (* 32 24))
(defasmbyte dirty)

(defasmproc init-tilemap {:page :code}
  [:xor :a]
  [:ld [dirty] :a]
  [:ret])

(defasmproc load-patterns {:page :code}
  ;; HL=patterns
  [:push :hl]
  [:push :hl]
  [:ld :hl (* 32 8 8 0)]
  [:call bios/SETWRT]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:push :hl]
  [:ld :hl (* 32 8 8 1)]
  [:call bios/SETWRT]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:ld :hl (* 32 8 8 2)]
  [:call bios/SETWRT]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc load-colors {:page :code}
  ;; HL=colors
  [:push :hl]
  [:push :hl]
  [:ld :hl (+ 0x2000 (* 32 8 8 0))]
  [:call bios/SETWRT]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:push :hl]
  [:ld :hl (+ 0x2000 (* 32 8 8 1))]
  [:call bios/SETWRT]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:ld :hl (+ 0x2000 (* 32 8 8 2))]
  [:call bios/SETWRT]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc clear-name {:page :code}
  [:ld :hl 0x1800]
  [:ld :bc (* 32 24)]
  [:xor :a]
  [:jp bios/FILVRM])

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


(defasmproc draw-horizontal-map {:page :code}
  ;; a=lines-page b=map-page iy=map-addr
  [:ld :a 1]
  [:ld [dirty] :a]

  ;; save map addr
  [:ld :hl tilemap-map]
  [:ld :a [:hl]]
  [:ld :iyl :a]
  [:inc :hl]
  [:ld :a [:hl]]
  [:ld :iyh :a]

  ;; write horizontal lines
  [:ld :hl offscreen]
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

(defasmproc update-offscreen {:page :code}
  [:ld :a [dirty]]
  [:or :a]
  [:ret :z]
  [:xor :a]
  [:ld [dirty] :a]
  [:di]

  ;; bc=32*height
  [:ld :a [tilemap-height]]
  [:ld :l :a]
  [:ld :h 0]
  (math/mul-hl-by-pow2 32)
  [:push :hl]
  [:pop :bc]
  ;; draw offscreen
  [:ld :hl offscreen]
  [:ld :de 0x1800]
  [:call bios/LDIRVM]
  [:ei]
  [:ret])

(defasmproc load-horizontal-map {:page :code}
  ;; a=lines-page b=map-page ix=map-addr

  ;; save pages
  [:ld [page-lines] :a]
  [:ld :a :b]
  [:ld [page-map] :a]

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
