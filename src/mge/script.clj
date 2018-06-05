(ns mge.script
  (:require [clj-z80.asm :refer :all :refer-macros :all]))


;; script vars

(defasmword arg0)
(defasmword arg1)
(defasmword arg2)
(defasmword arg3)

(def args [arg0 arg1 arg2 arg3])

(defasmword global0)
(defasmword global1)
(defasmword global2)
(defasmword global3)
(defasmword global4)
(defasmword global5)
(defasmword global6)
(defasmword global7)
(defasmword global8)
(defasmword global9)
(defasmword global10)
(defasmword global11)
(defasmword global12)
(defasmword global13)
(defasmword global14)
(defasmword global15)
(defasmword global16)
(defasmword global17)
(defasmword global18)
(defasmword global19)

(def globals [global0 global1 global2 global3 global4
              global5 global6 global7 global8 global9
              global10 global11 global12 global13 global14
              global15 global16 global17 global18 global19])


;; core

(defasmproc init-scripts {:page :code}
  [:xor :a]
  (map (fn [arg] [:ld [arg] :a]) args)
  (map (fn [global] [:ld [global] :a]) globals)
  [:ret])
