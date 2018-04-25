(ns mge.script
  (:require [mge.sprites :as spr]
            [mge.keys :as keys]
            [mge.title :as title]
            [clj-z80.asm :refer :all :refer-macros :all]
            [clojure.string :as str]))


;; script vars

(defasmbyte arg0)
(defasmbyte arg1)
(defasmbyte arg2)
(defasmbyte arg3)

(def args [arg0 arg1 arg2 arg3])

(defasmbyte global0)
(defasmbyte global1)
(defasmbyte global2)
(defasmbyte global3)
(defasmbyte global4)
(defasmbyte global5)
(defasmbyte global6)
(defasmbyte global7)
(defasmbyte global8)
(defasmbyte global9)
(defasmbyte global10)
(defasmbyte global11)
(defasmbyte global12)
(defasmbyte global13)
(defasmbyte global14)
(defasmbyte global15)
(defasmbyte global16)
(defasmbyte global17)
(defasmbyte global18)
(defasmbyte global19)

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
