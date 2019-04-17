(ns mge.graphics-screen2
  (:require [clojure.string :as str]
            [mikera.image.core :refer :all]
            [mge.graphics :refer :all]))

(defn- extract-row-colors
  [row]
  (let [colors (into {} (map-indexed (fn [i c] [c i]) (sort (distinct row))))]
    (when (> (count (keys colors)) 2)
      (throw (Exception. "Too many colors")))
    colors))

(defn- convert-row-to-colors
  [row]
  (let [colors (-> row extract-row-colors keys vec)]
    (bit-or (bit-shift-left (get colors 1 0) 4)
            (get colors 0 0))))

(defn- convert-row-to-patterns
  [row]
  (let [colors (extract-row-colors row)]
    (BigInteger. (str/join (map #(str (get colors %)) row)) 2)))

(defn- convert-tiles-to-colors
  [tiles]
  (flatten
   (map (fn [y]
          (map (fn [x]
                 (map convert-row-to-colors x))
               y))
        tiles)))

(defn- convert-tiles-to-patterns
  [tiles]
  (flatten
   (map (fn [y]
          (map (fn [x]
                 (map convert-row-to-patterns x))
               y))
        tiles)))

(defn- get-tile-image
  [image off-x off-y color-map]
  (for [y (range 8)]
    (for [x (range 8)]
      (let [rgba (.getRGB image (int (+ off-x x)) (int (+ off-y y)))]
        (get color-map rgba 0)))))
  

(defn- split-image-into-tiles
  [image color-map]
  (let [w (width image)
        h (height image)]
    (when-not (and (= (mod w 8) 0) (= (mod h 8) 0))
      (throw (Exception. (str "Image must be multiple of 8x8 pixels"))))
    (for [y (range 0 h 8)]
      (for [x (range 0 w 8)]
        (get-tile-image image x y color-map)))))

(defn convert-screen2
  [filename type]
  (let [image          (load-image filename)
        color-map      (extract-image-msx1-colors image)
        tiles          (split-image-into-tiles image color-map)
        image-colors   (convert-tiles-to-colors tiles)
        image-patterns (convert-tiles-to-patterns tiles)]
    [image-colors
     image-patterns]))
