(ns mge.debug
  (:require [clj-z80.asm :refer :all :refer-macros :all]))

(defasmproc print-a {:page :code}
  [:push :af]
  [:push :bc]
  [:ld :b :a]
  [:ld :a 0x60]
  [:out [0x2e] :a]
  [:ld :a :b]
  [:out [0x2f] :a]
  [:pop :bc]
  [:pop :af]
  [:ret])
