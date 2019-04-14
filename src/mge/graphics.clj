(ns mge.graphics
  (:require [clojure.string :as str]
            [mikera.image.core :refer :all]))


;; image

(defn- get-tile-image
  [image off-x off-y pixel-fn]
  (for [y (range 8)]
    (for [x (range 8)]
      (let [pixel (.getRGB image (int (+ off-x x)) (int (+ off-y y)))]
        (pixel-fn pixel)))))
  

(defn- split-image-into-tiles
  [image pixel-fn]
  (let [w (width image)
        h (height image)]
    (when-not (and (= (mod w 8) 0) (= (mod h 8) 0))
      (throw (Exception. (str "Image must be multiple of 8x8 pixels"))))
    (for [y (range 0 h 8)]
      (for [x (range 0 w 8)]
        (get-tile-image image x y pixel-fn)))))


;; msx1 colors

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

(defn get-msx1-color-index
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

(defn- extract-image-msx1-colors
  [image]
  (->> image get-pixels distinct
       (map (fn [pixel]
              (let [rgba      (get-pixel-rgba pixel)
                    msx-color (get-msx1-color-index rgba)]
                [pixel msx-color])))
       (into {})))


;; screen 2

(defn- extract-row-screen2-colors
  [row]
  (let [colors (into {} (map-indexed (fn [i c] [c i]) (sort (distinct row))))]
    (when (> (count (keys colors)) 2)
      (throw (Exception. "Too many colors")))
    colors))

(defn- convert-row-into-screen2-colors
  [row]
  (let [colors (-> row extract-row-screen2-colors keys vec)]
    (bit-or (bit-shift-left (get colors 1 0) 4)
            (get colors 0 0))))

(defn- convert-row-into-screen2-patterns
  [row]
  (let [colors (extract-row-screen2-colors row)]
    (BigInteger. (str/join (map #(str (get colors %)) row)) 2)))

(defn convert-screen2
  [filename type]
  (let [image            (load-image filename)
        image-colors     (extract-image-msx1-colors image)
        color-conversion #(get image-colors % 0)
        tiles            (split-image-into-tiles image color-conversion)
        image-colors     (flatten
                          (map (fn [y]
                                 (map (fn [x]
                                        (map convert-row-into-screen2-colors x))
                                      y))
                               tiles))
        image-patterns   (flatten
                          (map (fn [y]
                                 (map (fn [x]
                                        (map convert-row-into-screen2-patterns x))
                                      y))
                               tiles))]
    [image-colors
     image-patterns]))


;; sprites

(defn convert-sprite-16x16
  [filename]
  (let [image           (load-image filename)
        w               (width image)
        h               (height image)]
    (when-not (and (= w 16) (= h 16))
      (throw (Exception. (str "Sprite image `" image "' must have 16x16 size"))))
    (let [image-colors    (extract-image-msx1-colors image)
          final-colors    (vec (remove zero? (sort (vals image-colors))))
          pixels          (get-pixels image)
          parse-row       (fn [row] (BigInteger. (apply str row) 2))
          convert-part    (fn [x-offset color]
                            (map (fn [y]
                                   (->> (range 8)
                                        (map #(+ % x-offset (* (height image) y)))
                                        (map #(get pixels %))
                                        (map #(get image-colors %))
                                        (map #(if (= % color) 1 0))
                                        parse-row))
                                 (range h)))
          convert-pattern (fn [color]
                            (vec
                             (concat (convert-part 0 color)
                                     (convert-part 8 color))))
          final-patterns  (doall (mapv convert-pattern final-colors))]
      {:colors   final-colors
       :patterns final-patterns})))
