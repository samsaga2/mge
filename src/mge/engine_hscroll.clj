(ns mge.engine-hscroll
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.engine-offscreen :as off]
            [mge.engine-tilemap :as tilemap]))

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

  ;; patterns
  [:ld :a [:ix 4]] ;; pattern page
  [:ld :l [:ix 5]] ;; pattern low byte address
  [:ld :h [:ix 6]] ;; pattern high byte address
  [:ld :c [:ix 7]] ;; color page
  [:ld :e [:ix 8]] ;; color low byte address
  [:ld :d [:ix 9]] ;; color high byte address
  [:push :bc]
  [:push :de]
  (set-konami5-page 3 :a)
  [:call tilemap/load-patterns]

  ;; colors
  [:pop :hl]
  [:pop :bc]
  [:ld :a :c]
  (set-konami5-page 3 :a)
  [:jp tilemap/load-colors])


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
         (set-konami5-page 3 [tilemap/page-map])
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
  [:ld [tilemap/page-map] :a]
  [:ld :a :c]
  [:ld [page-types] :a]

  ;; save types addr
  [:ld [tilemap/tilemap-types] :hl]

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
