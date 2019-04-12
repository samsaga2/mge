(ns mge.engine-util
  (:require [clj-z80.asm :refer :all :refer-macros :all]))

(defasmproc call-hl {:page :code}
  [:jp [:hl]])

(defasmword rand-seed)

(defasmproc random-word {:page :code}
  [:push :bc]
  [:ld :hl [rand-seed]]
  [:ld :c :l]
  [:ld :b :h]
  [:add :hl :hl]
  [:add :hl :bc]
  [:add :hl :hl]
  [:add :hl :bc]
  [:add :hl :hl]
  [:add :hl :bc]
  [:add :hl :hl]
  [:add :hl :hl]
  [:add :hl :hl]
  [:add :hl :hl]
  [:add :hl :bc]
  [:inc :h]
  [:inc :hl]
  [:ld [rand-seed] :hl]
  [:pop :bc]
  [:ret])

(defasmproc signed-dcompr {:page :code}
  ;; http://karoshi.auic.es/index.php?topic=2316.0;wap2
  [[:ld :a :h]
   [:xor 0x80]
   [:ld :h :a]
   [:ld :a :d]
   [:xor 0x80]
   [:ld :d :a]
   [:sbc :hl :de]
   [:ret]])
