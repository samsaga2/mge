(ns mge.sprites
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sprites :as spr]
            [mge.util :as u]
            [mge.math :as m]))


;; sprite struct
(def +spr-type+ 0)
(def +spr-x+ 1)
(def +spr-y+ 2)
(def +spr-color+ 3)
(def +spr-w+ 4)
(def +spr-h+ 5)

;; sprites data
(def +sprites-count+ 32)
(def +varsprites-count+ 32)


;; sprite data

(defasmvar data (* +sprites-count+ +varsprites-count+))

(defasmproc clear-data {:page :code}
  ;; clear
  [:ld :hl data]
  [:ld [:hl] 0]
  [:ld :de data]
  [:inc :de]
  [:ld :bc (dec (* +sprites-count+ +varsprites-count+))]
  [:ldir]
  ;; default values
  [:ld :ix spr/spr-attributes]
  [:ld :de +varsprites-count+]
  [:ld :b 32]
  (label :loop
         [:ld [:ix +spr-y+] 212]
         [:ld [:ix +spr-w+] 8]
         [:ld [:ix +spr-h+] 8]
         [:add :ix :de]
         [:djnz :loop])
  [:ret])


;; attributes

(defasmproc clear-attributes {:page :code}
  [:ld :hl spr/spr-attributes]
  [:ld :bc 32]
  [:ld :a 0]
  (label :loop
         [:ld [:hl] 212]
         [:inc :hl]
         [:inc :hl]
         [:ld [:hl] :a]                 ; spr pattern
         [:inc :hl]
         [:ld [:hl] 15]                 ; spr color
         [:inc :hl]
         [:add 4]
         [:djnz :loop])
  [:ret])

(defasmproc update-attributes {:page :code}
  [:ld :ix data]
  [:ld :hl spr/spr-attributes]
  [:ld :de +varsprites-count+]
  [:ld :b +sprites-count+]
  (label :loop
         [:ld :a [:ix +spr-y+]]
         [:ld [:hl] :a]                 ; y
         [:inc :hl]

         [:ld :a [:ix +spr-x+]]
         [:ld [:hl] :a]                 ; x
         [:inc :hl]

         [:inc :hl]                     ; pattern

         [:ld :a [:ix +spr-color+]]     ; color
         [:ld [:hl] :a]
         [:inc :hl]

         [:add :ix :de]
         [:djnz :loop])
  [:jp spr/write-attributes])


;; sprites table

(defasmvar table (* +sprites-count+ 2))

(defasmbyte spr-selected)

(defasmproc init-table {:page :code}
  [:ld :hl table]
  [:ld [:hl] 0]
  [:ld :de table]
  [:inc :de]
  [:ld :bc (dec (* +sprites-count+ 2))]
  [:ldir]
  [:ret])

(defasmproc new-sprite {:page :code}
  ;; HL=init-func
  ;; DE=update-func
  [:ld :a [spr-selected]]
  [:push :af]
  [:push :hl]
  [:push :de]
  [:ld :hl table]
  [:ld :b +sprites-count+]
  [:ld :ix data]
  [:xor :a]
  [:ld [spr-selected] :a]
  (label :loop
         ;; get update addr
         [:ld :e [:hl]]
         [:inc :hl]
         [:ld :d [:hl]]
         [:inc :hl]

         [:ld :a :d]
         [:or :a]
         [:jp :nz :next]

         ;; free entry found
         [:pop :de]                     ; set update addr
         [:dec :hl]
         [:ld [:hl] :d]
         [:dec :hl]
         [:ld [:hl] :e]
         [:pop :hl]                     ; call init
         [:ld [:ix +spr-w+] 8]
         [:ld [:ix +spr-h+] 8]
         [:call u/call-hl]
         [:pop :af]                     ; restore selected
         [:ld [spr-selected] :a]
         [:ret]

         (label :next
                ;; next index
                [:ld :a [spr-selected]]
                [:inc :a]
                [:ld [spr-selected] :a]
                ;; next data
                [:ld :de +varsprites-count+]
                [:add :ix :de]
                [:djnz :loop]))
  ;; no free entry found, clear stack
  [:pop :de]
  [:pop :hl]
  ;; restore selected
  [:pop :af]
  [:ld [spr-selected] :a]
  [:ret])

(defasmproc delete-sprite {:page :code}
  ;; hide psrite
  [:ld [:ix +spr-y+] 212]
  ;; clear update table entry
  [:ld :a [spr-selected]]
  [:ld :l :a]
  [:ld :h 0]
  (m/mul-hl-by-pow2 2)
  [:ld :de table]
  [:add :hl :de]
  [:ld [:hl] 0]
  [:inc :hl]
  [:ld [:hl] 0]
  [:ret])

(defasmproc update-table {:page :code}
  [:ld :hl table]
  [:ld :b +sprites-count+]
  [:ld :ix data]
  [:xor :a]
  [:ld [spr-selected] :a]
  (label :loop
         ;; get update addr
         [:ld :e [:hl]]
         [:inc :hl]
         [:ld :d [:hl]]
         [:inc :hl]

         [:ld :a :d]
         [:or :a]
         [:jp :z :next]

         ;; call update script
         [:push :bc]
         [:push :hl]
         [:push :ix]
         [:ex :de :hl]
         [:call u/call-hl]
         [:pop :ix]
         [:pop :hl]
         [:pop :bc]

         (label :next
                ;; next index
                [:ld :a [spr-selected]]
                [:inc :a]
                [:ld [spr-selected] :a]
                ;; next data
                [:ld :de +varsprites-count+]
                [:add :ix :de]
                [:djnz :loop]))
  [:ret])


;; collision

(defasmword check-y1)
(defasmword check-y2)
(defasmword check-x1)
(defasmword check-x2)
(defasmword check-w)
(defasmword check-h)

;; a=1 if distance (x1,y1) to (x2,y2) < (w,h)
(defasmproc check-collision {:page :code}
  [:xor :a]
  ;; check y
  (label :check-y
         [:ld :hl [check-y1]]
         [:ld :de [check-y2]]
         [:or :a]
         [:sbc :hl :de]
         [:jp :c :swapy]
         [:ld :de [check-h]]
         [:call u/negate-de]
         [:add :hl :de]
         [:jp :c :no-collision]
         [:jp :check-x]
         (label :swapy
                [:ld :de [check-h]]
                [:add :hl :de]
                [:jp :nc :no-collision]))
  (label :check-x
         [:ld :hl [check-x1]]
         [:ld :de [check-x2]]
         [:or :a]
         [:sbc :hl :de]
         [:jp :c :swapx]
         [:ld :de [check-w]]
         [:call u/negate-de]
         [:add :hl :de]
         [:jp :c :no-collision]
         [:jp :collision]
         (label :swapx
                [:ld :de [check-w]]
                [:add :hl :de]
                [:jp :nc :no-collision]))
  (label :collision
         [:ld :a 1]
         [:or :a]
         [:ret])
  (label :no-collision
         [:xor :a]
         [:ret]))

(defasmproc collide {:page :code}
  [:ld :hl 0]
  [:ld [check-x1] :hl]
  [:ld [check-x2] :hl]
  [:ld [check-y1] :hl]
  [:ld [check-y2] :hl]
  [:ld [check-w] :hl]
  [:ld [check-h] :hl]
  [:ld :c :a]
  [:ld :hl table]
  [:ld :b +sprites-count+]
  [:ld :iy data]
  (label :loop
         ;; get update addr
         [:inc :hl]
         [:ld :d [:hl]]
         [:inc :hl]

         [:ld :a :d]
         [:or :a]
         [:jp :z :next]

         ;; check type
         [:ld :a [:iy +spr-type+]]
         [:cp :c]
         [:jp :nz :next]

         ;; check collision
         [:push :hl]
         [:ld :a [:ix +spr-x+]]         ; x1
         [:add 8]
         [:ld [check-x1] :a]
         [:ld :a [:ix +spr-y+]]         ; y1
         [:add 8]
         [:ld [check-y1] :a]
         [:ld :a [:iy +spr-x+]]         ; x2
         [:add 8]
         [:ld [check-x2] :a]
         [:ld :a [:iy +spr-y+]]         ; y2
         [:add 8]
         [:ld [check-y2] :a]
         [:ld :a [:ix +spr-w+]]         ; w
         [:add [:iy +spr-w+]]
         [:ld [check-w] :a]
         [:ld :a [:ix +spr-h+]]         ; h
         [:add [:iy +spr-h+]]
         [:ld [check-h] :a]

         [:call check-collision]
         [:pop :hl]
         [:ret :nz]

         (label :next
                ;; next data
                [:ld :de +varsprites-count+]
                [:add :iy :de]
                [:djnz :loop]))
  [:xor :a]
  [:ret])


;; sprite

(defasmproc write-pattern {:page :code}
  ;; spr-selected
  ;; HL=pattern addr
  [[:ld :a [spr-selected]]
   [:jp spr/write-pattern]])


;; core

(defasmproc init-sprites {:page :code}
  [:call spr/enable-sprites-16]
  [:call init-table]
  [:call clear-data]
  [:jp clear-attributes])

(defasmproc update-sprites {:page :code}
  [:call update-table]
  [:jp update-attributes])
