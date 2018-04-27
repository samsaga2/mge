(ns mge.sprites
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sprites :as spr]
            [mge.util :as u]
            [mge.math :as m]
            [clj-z80.msx.lib.sysvars :as sysvars]))


;; sprite struct
(def +spr-type+ 0)
(def +spr-flags+ 1)
(def +spr-x+ 2)
(def +spr-y+ 3)
(def +spr-color+ 4)
(def +spr-w+ 5)
(def +spr-h+ 6)

;; sprites consts
(def +flag-deleted+ 1)
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

  ;; save things
  [:ld :a [spr-selected]]
  [:push :af]
  [:push :ix]
  ;; save funcs on stack
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
         [:ld [:ix +spr-type+] 0]
         [:ld [:ix +spr-flags+] 0]
         [:ld [:ix +spr-w+] 8]
         [:ld [:ix +spr-h+] 8]
         [:call u/call-hl]
         [:pop :ix]                     ; restore things
         [:pop :af]
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
  [:pop :ix]
  [:pop :af]
  [:ld [spr-selected] :a]
  [:ret])

(defasmproc delete-sprite {:page :code}
  [:ld :a [:ix +spr-flags+]]
  [:or +flag-deleted+]
  [:ld [:ix +spr-flags+] :a]
  [:ret])

(defasmproc update-deleted {:page :code}
  [:ld :hl table]
  [:ld :b +sprites-count+]
  [:ld :ix data]
  (label :loop
         ;; get update addr
         [:inc :hl]
         [:ld :d [:hl]]
         [:inc :hl]

         [:ld :a :d]
         [:or :a]
         [:jp :z :next]

         ;; check deleted flag
         [:ld :a [:ix +spr-flags+]]
         [:and +flag-deleted+]
         [:jp :z :next]

         ;; real delete
         [:ld [:ix +spr-y+] 212]
         [:dec :hl]
         [:dec :hl]
         [:ld [:hl] 0]
         [:inc :hl]
         [:ld [:hl] 0]
         [:inc :hl]

         (label :next
                ;; next data
                [:ld :de +varsprites-count+]
                [:add :ix :de]
                [:djnz :loop]))
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

(defasmbyte vdps0)

;; a=1 if distance (x1,y1) to (x2,y2) < (w,h)
(defasmproc box-collision {:page :code}
  ;; w
  [:ld :a [:ix +spr-w+]]
  [:add [:iy +spr-w+]]
  [:ld :d :a]

  ;; x
  [:ld :a [:ix +spr-x+]]
  [:sub [:iy +spr-x+]]
  [:jp :nc :no-swap-x]
  [:neg]
  (label :no-swap-x)
  [:cp :d]
  [:jp :nc :no-collision]

  ;; h
  [:ld :a [:ix +spr-h+]]
  [:add [:iy +spr-h+]]
  [:ld :d :a]

  ;; y
  [:ld :a [:ix +spr-y+]]
  [:sub [:iy +spr-y+]]
  [:jp :nc :no-swap-y]
  [:neg]
  (label :no-swap-y)
  [:cp :d]
  [:jp :nc :no-collision]

  (label :collision
         [:xor :a]
         [:inc :a]
         [:ret])
  (label :no-collision
         [:xor :a]
         [:ret]))

(defasmproc collide {:page :code}
  [:ld :c :a]                           ; save type

  [:ld :a [vdps0]]                      ; check vdp collision bit
  [:bit 5 :a]
  [:ret :z]

  [:ld :hl table]                       ; check sprites box collision
  [:ld :b +sprites-count+]
  [:ld :iy data]
  (label :loop
         ;; get update addr
         [:inc :hl]
         [:ld :a [:hl]]
         [:inc :hl]

         [:or :a]
         [:jp :z :next]

         ;; check type
         [:ld :a [:iy +spr-type+]]
         [:cp :c]
         [:jp :nz :next]

         ;; check collision
         [:call box-collision]
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
  [:call update-deleted]
  [:jp update-attributes])
