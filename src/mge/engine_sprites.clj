(ns mge.engine-sprites
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [clj-z80.msx.lib.sprites :as spr]
            [clj-z80.image :refer [get-label]]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [mge.engine-util :as u]
            [mge.engine-math :as m]
            [mge.engine-tilemap :as tilemap]))


;; sprite struct
(def +spr-type+ 0)
(def +spr-flags+ 2)
(def +spr-x+ 4)
(def +spr-y+ 6)
(def +spr-color1+ 8)
(def +spr-color2+ 10)
(def +spr-w+ 12)
(def +spr-h+ 14)
(def +spr-anim+ 16)
(def +spr-anim-page+ 18)
(def +spr-local0+ 20)

;; sprites consts
(def +flag-deleted+ 1)
(def +sprites-count+ 16)
(def +varsprites-count+ 64)


;; variables

(defasmbyte vdps0)
(defasmbyte spr-flicker)
(defasmbyte spr-selected)
(defasmvar table (* +sprites-count+ 2))
(defasmvar data (* +sprites-count+ +varsprites-count+))


;; sprite data

(defasmproc clear-data {:page :code}
  ;; clear
  [:ld :hl data]
  [:ld [:hl] 0]
  [:ld :de data]
  [:inc :de]
  [:ld :bc (dec (* +sprites-count+ +varsprites-count+))]
  [:ldir]
  ;; default values
  [:ld :ix data]
  [:ld :de +varsprites-count+]
  [:ld :b +sprites-count+]
  [:ld :hl (* 192 8)]
  (label :loop
         [:ld [:ix +spr-y+] :l]
         [:ld [:ix (inc +spr-y+)] :h]
         [:add :ix :de]
         [:djnz :loop])
  [:ret])


;; attributes

(defasmproc update-attributes-asc {:page :code}
  [:ld :a 1]
  [:ld [spr-flicker] :a]
  [:ld :ix data]
  [:ld :iy spr/spr-attributes]
  [:ld :b +sprites-count+]
  [:ld :c 0]
  (label :loop
         ;; sprite 1
         [:ld :a [:ix +spr-color1+]]
         [:or :a]
         [:jp :z :hide-sprite1]
         [:ld [:iy spr/+attribute-color+] :a]

         [:ld :l [:ix +spr-y+]]
         [:ld :h [:ix (inc +spr-y+)]]
         (m/div-hl-by-pow2 8)
         [:ld [:iy spr/+attribute-y+] :l]
         [:ld [:iy (+ spr/+attribute-y+ 4)] :l]

         [:ld :l [:ix +spr-x+]]
         [:ld :h [:ix (inc +spr-x+)]]
         (m/div-hl-by-pow2 8)
         [:ld [:iy spr/+attribute-x+] :l]
         [:ld [:iy (+ spr/+attribute-x+ 4)] :l]

         [:ld :a :c]
         [:ld [:iy spr/+attribute-pattern+] :a]
         [:add 4]
         [:ld [:iy (+ spr/+attribute-pattern+ 4)] :a]
         [:add 4]
         [:ld :c :a]

         ;; sprite 2
         [:ld :a [:ix +spr-color2+]]
         [:or :a]
         [:jp :z :hide-sprite2]

         (label :show-sprite
                [:ld [:iy (+ spr/+attribute-color+ 4)] :a]
                [:jp :next])
         (label :hide-sprite1
                [:ld [:iy spr/+attribute-y+] 192])
         (label :hide-sprite2
                [:ld [:iy (+ spr/+attribute-y+ 4)] 192])
         (label :next
                [:ld :de +varsprites-count+]
                [:add :ix :de]
                [:ld :de 8]
                [:add :iy :de]
                [:djnz :loop]))
  [:jp spr/write-attributes])

(defasmproc update-attributes-desc {:page :code}
  [:xor :a]
  [:ld [spr-flicker] :a]
  [:ld :ix data]
  [:ld :iy (fn [] (+ (:address (get-label spr/spr-attributes))
                     (* 30 4)))]
  [:ld :b +sprites-count+]
  [:ld :c 0]
  (label :loop
         ;; sprite 1
         [:ld :a [:ix +spr-color1+]]
         [:or :a]
         [:jp :z :hide-sprite1]
         [:ld [:iy spr/+attribute-color+] :a]


         [:ld :l [:ix +spr-y+]]
         [:ld :h [:ix (inc +spr-y+)]]
         (m/div-hl-by-pow2 8)
         [:ld [:iy spr/+attribute-y+] :l]
         [:ld [:iy (+ spr/+attribute-y+ 4)] :l]

         [:ld :l [:ix +spr-x+]]
         [:ld :h [:ix (inc +spr-x+)]]
         (m/div-hl-by-pow2 8)
         [:ld [:iy spr/+attribute-x+] :l]
         [:ld [:iy (+ spr/+attribute-x+ 4)] :l]

         [:ld :a :c]
         [:ld [:iy spr/+attribute-pattern+] :a]
         [:add 4]
         [:ld [:iy (+ spr/+attribute-pattern+ 4)] :a]
         [:add 4]
         [:ld :c :a]

         ;; sprite 2
         [:ld :a [:ix +spr-color2+]]
         [:or :a]
         [:jp :z :hide-sprite2]

         (label :show-sprite
                [:ld [:iy (+ spr/+attribute-color+ 4)] :a]
                [:jp :next])
         (label :hide-sprite1
                [:ld [:iy spr/+attribute-y+] 192])
         (label :hide-sprite2
                [:ld [:iy (+ spr/+attribute-y+ 4)] 192])
         (label :next
                [:ld :de +varsprites-count+]
                [:add :ix :de]
                [:ld :de -8]
                [:add :iy :de]
                [:djnz :loop]))
  [:jp spr/write-attributes])

(defasmproc update-attributes {:page :code}
  [:ld :a [spr-flicker]]
  [:or :a]
  [:jp :z update-attributes-asc]
  [:jp update-attributes-desc])


;; sprites table

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
         [:ld [:ix (inc +spr-type+)] 0]
         [:ld [:ix +spr-flags+] 0]
         [:ld [:ix (inc +spr-flags+)] 0]
         [:ld [:ix +spr-w+] 8]
         [:ld [:ix (inc +spr-w+)] 0]
         [:ld [:ix +spr-h+] 8]
         [:ld [:ix (inc +spr-h+)] 0]
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
         [:ld [:ix +spr-y+] (bit-and (* 192 8) 255)]
         [:ld [:ix (inc +spr-y+)] (bit-shift-right (* 192 8) 8)]
         [:ld [:ix (+ +spr-y+ 4)] (bit-and (* 192 8) 255)]
         [:ld [:ix (inc (+ +spr-y+ 4))] (bit-shift-right (* 192 8) 8)]
         [:ld [:ix +spr-anim+] 0]
         [:ld [:ix (inc +spr-anim+)] 0]
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

;; a=1 if distance (x1,y1) to (x2,y2) < (w,h)

(defasmproc box-collision {:page :code}
  ;; w
  [:ld :a [:ix +spr-w+]]
  [:add [:iy +spr-w+]]
  [:ld :c :a]

  ;; x
  [:ld :l [:ix +spr-x+]]
  [:ld :h [:ix (inc +spr-x+)]]
  (m/div-hl-by-pow2 8)
  [:ld :a :l]


  [:ld :e [:iy +spr-x+]]
  [:ld :d [:iy (inc +spr-x+)]]
  (m/div-de-by-pow2 8)
  [:sub :e]

  [:jp :nc :no-swap-x]
  [:neg]
  (label :no-swap-x)
  [:cp :c]
  [:jp :nc :no-collision]

  ;; h
  [:ld :a [:ix +spr-h+]]
  [:add [:iy +spr-h+]]
  [:ld :c :a]

  ;; y
  [:ld :l [:ix +spr-y+]]
  [:ld :h [:ix (inc +spr-y+)]]
  (m/div-hl-by-pow2 8)
  [:ld :a :l]

  [:ld :e [:iy +spr-y+]]
  [:ld :d [:iy (inc +spr-y+)]]
  (m/div-de-by-pow2 8)
  [:sub :e]

  [:jp :nc :no-swap-y]
  [:neg]
  (label :no-swap-y)
  [:cp :c]
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
         [:push :hl]
         [:push :bc]
         [:call box-collision]
         [:pop :bc]
         [:pop :hl]
         [:ret :nz]

         (label :next
                ;; next data
                [:ld :de +varsprites-count+]
                [:add :iy :de]
                [:djnz :loop]))
  [:xor :a]
  [:ret])


;; animation

(defasmproc animation-next-frame {:page :code}
  [[:pop :hl]
   [:ld [:ix +spr-anim+] :l]
   [:ld [:ix (inc +spr-anim+)] :h]
   [:ret]])

(defasmproc update-animations {:page :code}
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

         ;; get animation addr
         [:ld :e [:ix +spr-anim+]]
         [:ld :d [:ix (inc +spr-anim+)]]
         [:ld :a :d]
         [:or :a]
         [:jp :z :next]

         ;; call update script
         [:ld :a [:ix +spr-anim-page+]]
         (set-konami5-page 3 :a)
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


;; pattern

(defasmproc write-pattern {:page :code}
  ;; spr-selected
  ;; HL=pattern addr
  [[:di]
   [:ld :a [spr-selected]]
   [:add :a]
   [:push :hl]
   [:call bios/CALPAT]
   [:ex :de :hl]
   [:ld :bc 64]
   [:pop :hl]
   [:call bios/LDIRVM]
   [:ei]
   [:ret]])


;; tile

(defasmproc get-tile {:page :code}
  ;; in b=offset-x c=offset-y out type=a
  [:ld :l [:ix +spr-x+]]
  [:ld :h [:ix (inc +spr-x+)]]
  (m/div-hl-by-pow2 8)
  [:ld :a :l]
  [:add :b]
  [:ld :b :a]

  [:ld :l [:ix +spr-y+]]
  [:ld :h [:ix (inc +spr-y+)]]
  (m/div-hl-by-pow2 8)
  [:ld :a :l]
  [:add :c]
  [:ld :c :a]

  [:jp tilemap/get-tile])


;; core

(defasmproc init-sprites {:page :code}
  [:xor :a]
  [:ld [spr-flicker] :a]
  [:call spr/enable-sprites-16]
  [:call init-table]
  [:jp clear-data])

(defasmproc update-sprites {:page :code}
  [:call update-table]
  [:call update-animations]
  [:call update-deleted]
  [:jp update-attributes])
