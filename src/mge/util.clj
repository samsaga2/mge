(ns mge.util
  (:require [clj-z80.asm :refer :all :refer-macros :all]))

(defasmproc call-hl {:page :code}
  [:jp [:hl]])

(defasmproc negate-de {:page :code}
  [:xor :a]
  [:sub :e]
  [:ld :e :a]
  [:sbc :a]
  [:sub :d]
  [:ld :d :a]
  [:ret])
