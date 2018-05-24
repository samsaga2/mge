(ns mge.resources-tilemaps
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clj-z80.msx.util.graphics :refer [convert-screen2 convert-sprite-16x16]]
            [clj-z80.msx.util.compress :refer [compress-lz77]]
            [mge.resources-id :refer :all]
            [clj-z80.asm :refer :all :refer-macros :all]
            [clojure.string :as str]))


(def res-pages (vec (range 4 64)))


;; utils

(defn- get-attr
  [m id]
  (-> m :attrs id))

(defn- find-child-nodes
  [m tag]
  (filter #(= (:tag %) tag) (:content m)))

(defn- find-child-node
  [m tag]
  (first (find-child-nodes m tag)))

(defn- get-filename-with-path
  [orig-filename new-filename]
  (str (.resolve (.getParent (.toPath (io/file orig-filename)))
                 new-filename)))

(defn- try-parse-int
  [i]
  (try (Integer. i)
       (catch Exception e 0)))


;; load tilemap

(defn- get-tileset-types
  [m]
  (let [types (->> (find-child-nodes m :tile)
                   (map (fn [n]
                          (let [id   (get-attr n :id)
                                type (get-attr (first
                                                (filter #(and (= (:tag %) :property)
                                                              (= (get-attr % :name) "type"))
                                                        (:content (find-child-node n :properties))))
                                               :value)]
                            [(try-parse-int id)
                             (try-parse-int type)])))
                   (into {}))]
    (mapv #(get types % 0)
          (range 256))))

(defn- load-tileset
  [m filename]
  (if-let [source (get-attr (find-child-node m :tileset) :source)]
    (load-tileset (xml/parse (get-filename-with-path filename source)) filename)
    (let [name         (.getName (io/file filename))
          image-source (get-attr (find-child-node m :image) :source)
          image-source (get-filename-with-path filename image-source)]
      {:tilemap   image-source
       :tiletypes (get-tileset-types m)})))

(defn load-tilemap
  [filename]
  (let [m     (xml/parse filename)
        layer (find-child-node m :layer)
        w     (try-parse-int (get-attr layer :width))
        h     (try-parse-int (get-attr layer :height))
        data  (find-child-node layer :data)
        cells (mapv try-parse-int (str/split (str/join "," (:content data)) #","))]
    (merge {:tile-width  (try-parse-int (get-attr m :tilewidth))
            :tile-height (try-parse-int (get-attr m :tileheight))
            :width       w
            :height      h
            :cells       cells}
           (load-tileset m filename))))


;; make tilemap

(defn- make-tileset-image
  [tilemap filename]
  (let [image-source (:tilemap tilemap)
        [colors patterns] (convert-screen2 image-source :colors)
        patterns          (compress-lz77 patterns)
        colors            (compress-lz77 colors)]
    (println "Compiled tilemap image" filename
             (+ (count patterns)
                (count colors))
             "bytes")
    (make-proc (make-tilemap-id filename :pattern) res-pages
               [[:db patterns]])
    (make-proc (make-tilemap-id filename :colors) res-pages
               [[:db colors]])))

(defn- make-tileset-type
  [tilemap filename]
  (let [types   (:tiletypes tilemap)
        id       (make-tilemap-id filename :types)]
    (println "Compiled tilemap types" filename
             (count types)
             "bytes")
    (make-proc id res-pages
               [[:db types]])))

(defn- make-tileset-horizontal-cells
  [tilemap filename]
  (let [w     (:width tilemap)
        h     (:height tilemap)
        cells (:cells tilemap)]
    (assert (< h 24))
    (let [lines (distinct (mapv (fn [x]
                                  (mapv (fn [y]
                                          (nth cells (+ x (* y w))))
                                        (range 0 h)))
                                (range 0 w)))]
      (assert (< (count lines) 256))
      (let [map-lines (mapv (fn [x]
                              (let [line (mapv (fn [y]
                                                 (nth cells (+ x (* y w))))
                                               (range 0 h))]
                                (.indexOf lines line)))
                            (range 0 w))]
        (println "Compiled tilemap cells" filename
                 (+ (* (count lines) h)
                    w))
        (make-proc (make-tilemap-id filename :attr) res-pages
                   [(dw w h)])
        (make-proc (make-tilemap-id filename :lines) res-pages
                   [[:db (apply concat lines)]])
        (make-proc (make-tilemap-id filename :map) res-pages
                   [[:db map-lines]])))))


;; core

(defn make-tilemap
  [filename]
  (let [tilemap (load-tilemap filename)]
    (assert (= 8 (:tile-width tilemap)))
    (assert (= 8 (:tile-height tilemap)))
    (make-tileset-image tilemap filename)
    (make-tileset-type tilemap filename)
    (make-tileset-horizontal-cells tilemap filename)))
