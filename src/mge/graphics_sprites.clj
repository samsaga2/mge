(ns mge.graphics-sprites
  (:require [clojure.string :as str]
            [mikera.image.core :refer :all]
            [mge.graphics :refer :all]))

(defn- parse-row
  "convert row to binary string"
  [row]
  (BigInteger. (apply str row) 2))

(defn- extract-piece
  "get a 8x16 image piece in form of row patterns"
  [x-offset color image image-colors]
  (let [pixels (get-pixels image)
        h      (height image)]
    (map (fn [y]
           (->> (range 8)
                (map #(+ % x-offset (* (height image) y)))
                (map #(get pixels %))
                (map #(get image-colors %))
                (map #(if (= % color) 1 0))
                parse-row))
         (range h))))

(defn- convert-pattern
  [color image image-colors]
  (vec
   (concat (extract-piece 0 color image image-colors)
           (extract-piece 8 color image image-colors))))

(defn- convert-sprite-colors
  "all the sprite colors sorted and without zero (transparency)"
  [image-colors]
  (vec
   (remove zero?
           (sort (vals image-colors))))) 

(defn- convert-sprite-patterns
  [image image-colors final-colors]
  (doall
   (mapv #(convert-pattern % image image-colors)
         final-colors))) 

(defn- assert-image-size
  "make sure that the image has 16x16 size"
  [image]
  (let [w (width image)
        h (height image)]
    (when-not (and (= w 16) (= h 16))
      (throw (Exception. (str "Sprite image `" image "' must have 16x16 size"))))))

(defn convert-sprite-16x16
  [filename]
  (let [image (load-image filename)]
    (assert-image-size image)
    (let [image-colors   (extract-image-msx1-colors image)
          final-colors   (convert-sprite-colors image-colors)
          final-patterns (convert-sprite-patterns image image-colors final-colors)]
          {:colors   final-colors
           :patterns final-patterns})))
