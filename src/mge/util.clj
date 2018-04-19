(ns mge.util
  (:require [clj-z80.asm :refer :all :refer-macros :all]))

(defasmproc call-hl {:page :code}
  [:jp [:hl]])
