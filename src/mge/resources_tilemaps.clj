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
  [tilemap]
  (let [tileset-filename  (:tilemap tilemap)
        [colors patterns] (convert-screen2 tileset-filename :colors)
        patterns          (compress-lz77 patterns)
        colors            (compress-lz77 colors)
        tileset-pat-id    (make-tilemap-id tileset-filename :pattern)
        tileset-col-id    (make-tilemap-id tileset-filename :colors)]
    (println "Compiled tileset" tileset-filename
             (+ (count patterns) (count colors))
             "bytes")
    (make-proc tileset-pat-id res-pages [[:db patterns]])
    (make-proc tileset-col-id res-pages [[:db colors]])
    {:tileset-patterns tileset-pat-id
     :tileset-colors   tileset-col-id}))


;; make tile types

(defn- make-tileset-type
  [tilemap filename]
  (let [types    (:tiletypes tilemap)
        id       (make-tilemap-id filename :types)]
    (println "Compiled tileset types" filename
             (count types)
             "bytes")
    (make-proc id res-pages [[:db types]])
    {:tileset-types types}))


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
  [tilemap filename tileset]
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
    (println "Compiled tilemap" filename
             (+ (* (count set-lines) h)
                (* w 2)
                8))
    (make-proc attr-id res-pages
               [;; attrs
                (dw w h)
                ;; patterns
                [:db (fn [] (:page (get-label (:tileset-patterns tileset))))]
                (dw (:tileset-patterns tileset))
                ;; colors
                [:db (fn [] (:page (get-label (:tileset-colors tileset))))]
                (dw (:tileset-colors tileset))])
    (make-proc lines-id res-pages [(apply db (apply concat set-lines))])
    (make-proc map-id res-pages [(apply dw map-lines)])))


;; core

(defn- make-tileset
  [tilemap filename]
  ;; check tile size
  (when-not (and (= 8 (:tile-width tilemap))
                 (= 8 (:tile-height tilemap)))
    (throw (Exception. (str "Tiles must be 8x8 pixels size"))))
  ;; compile tile image
  (merge (make-tileset-image tilemap)
         (make-tileset-type tilemap filename)))

(defn make-tilemap
  [file]
  (let [tilemap  (tiled/load-tilemap (.getPath file))
        filename (.getName file)
        tileset  (make-tileset tilemap filename)]
    ;; compile tilemap
    (case (:direction tilemap)
      "horizontal" (make-horizontal-tilemap tilemap filename tileset)
      (throw (Exception. "Unknown tilemap direction")))))
