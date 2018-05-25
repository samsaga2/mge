(ns mge.tilemap
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.image :refer [set-konami5-page]]))

(defasmword tilemap-width)
(defasmword tilemap-height)
(defasmword tilemap-map)
(defasmbyte page-lines)
(defasmbyte page-map)

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

(defasmproc load-horizontal-map {:page :code}
  ;; a=lines-page b=map-page ix=map-addr
  [:di]

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

  ;; write horizontal lines
  [:ld :hl 0x1800]
  [:ld :b 32]
  (label :x
         [:push :hl]
         [:push :bc]

         ;; get line addr
         (set-konami5-page 3 [page-map])
         [:ld :e [:ix 0]]
         [:ld :d [:ix 1]]
         [:inc :ix]
         [:inc :ix]

         ;; write vertical line
         (set-konami5-page 3 [page-lines])
         [:ld :a [tilemap-height]]
         [:ld :b :a]
         (label :y
                [:call bios/SETWRT]

                [:ld :a [:de]]
                [:inc :de]
                [:out [0x98] :a]

                [:push :de]
                [:ld :de 32]
                [:add :hl :de]
                [:pop :de]
                [:djnz :y])

         [:pop :bc]
         [:pop :hl]
         [:inc :hl]
         [:djnz :x])

  [:ei]
  [:ret])
