(ns mge.engine-math
  (:require [clj-z80.asm :refer :all :refer-macros :all]))

(defn- log2
  [n]
  (/ (Math/log n) (Math/log 2)))

(defn mul-hl-by-pow2
  [n]
  (let [n (log2 n)]
    (assert (= (Math/floor n) n))
    (repeat n [:add :hl :hl])))

(defn div-hl-by-pow2
  [n]
  (let [n (log2 n)]
    (assert (= (Math/floor n) n))
    (apply concat
           (repeat n [[:sra :h]
                      [:rr :l]]))))

(defn div-de-by-pow2
  [n]
  (let [n (log2 n)]
    (assert (= (Math/floor n) n))
    (apply concat
           (repeat n [[:sra :d]
                      [:rr :e]]))))

(defasmproc mult {:page :code}
  ;; TODO
  [:ret])

(defasmproc div {:page :code}
  ;; TODO
  [:ret])
