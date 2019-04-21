(ns mge.resources-tilemaps
  (:require [clojure.java.io :as io]
            [clj-z80.image :refer [get-label]]
            [clj-z80.msx.util.graphics :refer [convert-screen2 convert-sprite-16x16]]
            [clj-z80.msx.util.compress :refer [compress-lz77]]
            [mge.resources-id :refer :all]
            [clj-z80.asm :refer :all :refer-macros :all]
            [mge.tiled :as tiled]))


(def res-pages (vec (range 4 64)))


;; tileset

(defn- make-tileset-image
  [tilemap filename]
  (let [tileset-filename  (:tilemap tilemap)
        [colors patterns] (convert-screen2 tileset-filename :colors)
        patterns          (compress-lz77 patterns)
        colors            (compress-lz77 colors)
        patterns-id       (make-tilemap-id filename :pattern)
        colors-id         (make-tilemap-id filename :colors)]
    (println "Compiled tilemap image" filename
             (+ (count patterns) (count colors))
             "bytes")
    (make-proc patterns-id res-pages [[:db patterns]])
    (make-proc colors-id res-pages [[:db colors]])))


;; make tile types

(defn- make-tileset-type
  [tilemap filename]
  (let [types   (:tiletypes tilemap)
        id       (make-tilemap-id filename :types)]
    (println "Compiled tilemap types" filename
             (count types)
             "bytes")
    (make-proc id res-pages
               [[:db types]])))

;; make horizontal tilemap

(defn- get-vertical-lines
  [tilemap]
  (let [w     (:width tilemap)
        h     (:height tilemap)
        cells (:cells tilemap)]
    (mapv (fn [x]
            (mapv (fn [y]
                    (nth cells (+ x (* y w))))
                  (range h)))
          (range w)))) 

(defn- make-horizontal-tilemap
  [tilemap filename]
  (let [w         (:width tilemap)
        h         (:height tilemap)
        cells     (:cells tilemap)
        attr-id   (make-tilemap-id filename :attr)
        lines-id  (make-tilemap-id filename :lines)
        map-id    (make-tilemap-id filename :map)
        lines     (get-vertical-lines tilemap)
        set-lines (distinct lines)
        map-lines (mapv (fn [line]
                          (fn []
                            (+ (* (.indexOf set-lines line) h)
                               (:address (get-label lines-id)))))
                        lines)]
    (assert (<= h 24))
    (assert (<= (* (count set-lines) h) 0x2000))
    (println "Compiled tilemap cells" filename
             (+ (* (count set-lines) h)
                w))
    (make-proc attr-id res-pages [(dw w h)])
    (make-proc lines-id res-pages [(apply db (apply concat set-lines))])
    (make-proc map-id res-pages [(apply dw map-lines)])))


;; core

(defn make-tilemap
  [file]
  (let [tilemap  (tiled/load-tilemap (.getPath file))
        filename (.getName file)]
    ;; check tile size
    (when-not (and (= 8 (:tile-width tilemap))
                   (= 8 (:tile-height tilemap)))
      (throw (Exception. (str "Tiles must be 8x8 pixels size"))))
    ;; compile tile image
    (make-tileset-image tilemap filename)
    (make-tileset-type tilemap filename)
    ;; compile tilemap
    (case (:direction tilemap)
      "horizontal" (make-horizontal-tilemap tilemap filename)
      (throw (Exception. "Unknown tilemap direction")))))
