(ns mge.engine-tilemap
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clj-z80.msx.lib.uncompress :refer [uncompress-lz77-to-vram]]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.engine-offscreen :as off]
            [mge.engine-math :as math]
            [mge.engine-vdp :as vdp]))

(defasmword tilemap-types)
(defasmbyte page-map)

;; load tiles

(defasmproc load-patterns {:page :code}
  ;; HL=patterns
  [:push :hl]
  [:push :hl]
  [:ld :hl (* 32 8 8 0)]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:push :hl]
  [:ld :hl (* 32 8 8 1)]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:ld :hl (* 32 8 8 2)]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc load-colors {:page :code}
  ;; HL=colors
  [:push :hl]
  [:push :hl]
  [:ld :hl (+ 0x2000 (* 32 8 8 0))]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:push :hl]
  [:ld :hl (+ 0x2000 (* 32 8 8 1))]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:call uncompress-lz77-to-vram]
  [:pop :hl]

  [:push :hl]
  [:ld :hl (+ 0x2000 (* 32 8 8 2))]
  [:call vdp/set-write-addr]
  [:pop :hl]
  [:jp uncompress-lz77-to-vram])

(defasmproc clear-name {:page :code}
  [:ld :hl 0x1800]
  [:ld :bc (* 32 24)]
  [:xor :a]
  [:jp bios/FILVRM])


;; tiles

(defasmproc get-tile {:page :code}
  ;; in b=pixel-x c=pixel-y out tile=a

  ;; de=y/8*32
  [:ld :a :c]
  [:and (- 255 7)]
  [:ld :l :a]
  [:ld :h 0]
  (math/mul-hl-by-pow2 4)
  [:ex :de :hl]

  ;; hl=x/8
  [:ld :l :b]
  [:ld :h 0]
  (math/div-hl-by-pow2 8)
  [:add :hl :de]

  ;; hl=offscreen+y/8*32+x/8
  [:ld :de off/offscreen]
  [:add :hl :de]

  ;; get type
  (set-konami5-page 3 [page-map])
  [:ld :e [:hl]]
  [:ld :d 0]
  [:ld :hl [tilemap-types]]
  [:add :hl :de]
  [:ld :a [:hl]]

  [:ret])
