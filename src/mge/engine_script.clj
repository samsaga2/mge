(ns mge.engine-script
  (:require [clj-z80.asm :refer :all :refer-macros :all]))


;; script vars

(defasmword arg0)
(defasmword arg1)
(defasmword arg2)
(defasmword arg3)

(def args [arg0 arg1 arg2 arg3])
