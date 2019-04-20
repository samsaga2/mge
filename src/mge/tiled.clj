(ns mge.tiled
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]))


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

(defn- find-property-node
  [m name]
  (first
   (filter #(and (= (:tag %) :property)
                 (= (get-attr % :name) name))
           (:content (find-child-node m :properties)))))


;; load tilemap

(defn- get-tileset-types
  [m]
  (let [types (->> (find-child-nodes m :tile)
                   (map (fn [n]
                          (let [id   (get-attr n :id)
                                type (get-attr (find-property-node n "type")
                                               :value)]
                            [(try-parse-int id)
                             (try-parse-int type)])))
                   (into {}))]
    (mapv #(get types % 0)
          (range 256))))

(defn- load-tileset
  [m filename]
  (if-let [source (get-attr (find-child-node m :tileset) :source)]
    (load-tileset (xml/parse (get-filename-with-path filename source))
                  filename)
    (let [name         (.getName (io/file filename))
          image-source (get-attr (find-child-node m :image) :source)
          image-source (get-filename-with-path filename image-source)]
      {:tilemap   image-source
       :tiletypes (get-tileset-types m)})))

(defn load-tilemap
  [filename]
  (let [m         (xml/parse filename)
        direction (get-attr (find-property-node m "direction") :value)
        layer     (find-child-node m :layer)
        w         (try-parse-int (get-attr layer :width))
        h         (try-parse-int (get-attr layer :height))
        data      (find-child-node layer :data)
        cells     (str/join "" (map #(str/replace % #"(\n|\r|\s)+" "")
                                    (:content data)))
        cells     (->> (str/split cells #",")
                       (map try-parse-int)
                       (mapv dec))]
    (merge {:tile-width  (try-parse-int (get-attr m :tilewidth))
            :tile-height (try-parse-int (get-attr m :tileheight))
            :width       w
            :height      h
            :direction   direction
            :cells       cells}
           (load-tileset m filename))))
