(ns mge.graphics
  (:require [clojure.string :as str]
            [mikera.image.core :refer :all]))

(def msx1-colors
  [[0 0 0 0]
   [0 0 0 255]
   [0 241 20 255]
   [68 249 86 255]
   [85 79 255 255]
   [128 111 255 255]
   [250 80 51 255]
   [12 255 255 255]
   [255 81 52 255]
   [255 115 86 255]
   [226 210 4 255]
   [242 217 71 255]
   [4 212 19 255]
   [231 80 229 255]
   [208 208 208 255]
   [255 255 255 255]])

(defn- color-distance
  [[r1 g1 b1 a1] [r2 g2 b2 a2]]
  (if (not= a1 a2)
    Integer/MAX_VALUE
    (Math/sqrt (+ (Math/pow (- r1 r2) 2)
                  (Math/pow (- g1 g2) 2)
                  (Math/pow (- b1 b2) 2)))))

(defn- get-msx1-color-index
  [color]
  (->> msx1-colors
       (map-indexed (fn [i msx-color] [i (color-distance msx-color color)]))
       (sort-by second)
       first
       first))

(defn- get-pixel-rgba
  [pixel]
  (let [a (bit-and (bit-shift-right pixel 24) 0xff)
        r (bit-and (bit-shift-right pixel 16) 0xff)
        g (bit-and (bit-shift-right pixel 8) 0xff)
        b (bit-and (bit-shift-right pixel 0) 0xff)]
    [r g b a]))

(defn extract-image-msx1-colors
  [image]
  (->> image get-pixels distinct
       (map (fn [pixel]
              (let [rgba      (get-pixel-rgba pixel)
                    msx-color (get-msx1-color-index rgba)]
                [pixel msx-color])))
       (into {})))
