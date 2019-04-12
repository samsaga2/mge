(ns mge.engine-offscreen
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [mge.engine-math :as m]))


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

(defasmproc write-str {:page :code}
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

(defasmbyte write-num-skip)

(defasmproc write-num {:page :code}
  ;; a=x b=y de=num
  [:ld :l :b]
  [:ld :h 0]
  (m/mul-hl-by-pow2 32)

  [:ld :c :a]
  [:ld :b 0]
  [:add :hl :bc]

  [:ld :bc offscreen]
  [:add :hl :bc]

  [:ex :de :hl]

  [:ld :a 1]
  [:ld [dirty] :a]

  [:xor :a]
  [:ld [write-num-skip] :a]

  [:ld :bc -10000]
  [:call :num1]
  [:ld :bc -1000]
  [:call :num1]
  [:ld :bc -100]
  [:call :num1]
  [:ld :bc -10]
  [:call :num1]
  [:ld :bc -1]
  (label :num1
         [:ld :a 47])
  (label :num2
         [:inc :a]
         [:add :hl :bc]
         [:jr :c :num2]
         [:sbc :hl :bc]
         [:jp :put-char])
  (label :put-char
         [:cp 48]
         [:jp :nz :put-char-final]
         ;; skip zeros
         [:push :af]
         [:ld :a [write-num-skip]]
         [:or :a]
         [:jp :z :no-put-char]
         [:pop :af])

  (label :put-char-final
         [:ld [:de] :a]
         [:inc :de]
         [:ld :a 1]
         [:ld [write-num-skip] :a]
         [:ret])
  (label :no-put-char
         [:pop :af]
         [:ret]))


(defasmproc write-znum {:page :code}
  ;; a=x b=y de=num
  [:ld :l :b]
  [:ld :h 0]
  (m/mul-hl-by-pow2 32)

  [:ld :c :a]
  [:ld :b 0]
  [:add :hl :bc]

  [:ld :bc offscreen]
  [:add :hl :bc]

  [:ex :de :hl]

  [:ld :a 1]
  [:ld [dirty] :a]

  [:ld :bc -10000]
  [:call :num1]
  [:ld :bc -1000]
  [:call :num1]
  [:ld :bc -100]
  [:call :num1]
  [:ld :bc -10]
  [:call :num1]
  [:ld :bc -1]
  (label :num1
         [:ld :a 47])
  (label :num2
         [:inc :a]
         [:add :hl :bc]
         [:jr :c :num2]
         [:sbc :hl :bc]
         [:jp :put-char])
  (label :put-char
         [:ld [:de] :a]
         [:inc :de]
         [:ret]))
