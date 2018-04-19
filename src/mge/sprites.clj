(ns mge.sprites
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sprites :as spr]
            [mge.util :as u]
            [mge.math :as m]))


;; sprite struct
(def +spr-state+ 0)
(def +spr-x+ 1)
(def +spr-y+ 2)
(def +spr-color+ 3)

;; sprite state field
(def +state-unused+ 0)
(def +state-visible+ 1)
(def +state-hidden+ 2)

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
         [:ld [:ix +spr-x+] 212]
         [:add :ix :de]
         [:djnz :loop])
  [:ret])


;; attributes

(defasmproc clear-attributes {:page :code}
  [:ld :hl spr/spr-attributes]
  [:ld :bc 32]
  [:ld :a 0]
  (label :loop
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
