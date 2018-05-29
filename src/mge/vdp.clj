(ns mge.vdp
  (:require [clj-z80.asm :refer :all :refer-macros :all]))

(defasmproc set-write-addr {:page :code}
  [:ld :a :l]
  [:out [0x99] :a]
  [:ld :a :h]
  [:and 0x3F]
  [:or 0x40]
  [:out [0x99] :a]
  [:ret])

(defasmproc clear-name-table {:page :code}
  [:ld :hl 0x1800]
  [:call set-write-addr]
  [:ld :b 3]
  [:xor :a]
  (label :loop
         [:out [0x98] :a]
         [:inc :a]
         [:jp :nz :loop]
         [:djnz :loop])
  [:ret])
