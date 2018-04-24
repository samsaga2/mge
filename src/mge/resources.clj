(ns mge.resources
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.image :refer [set-label!]]
            [clojure.java.io :as io]
            [clj-z80.msx.util.sprites :refer [convert-sprite-16x16]]
            [clj-z80.msx.util.graphics :refer [convert-screen2]]
            [clj-z80.msx.util.compress :refer [compress-lz77]]
            [mge.script-compile :as sc]
            [clojure.string :as str]))


;; ids

(defn make-sprite-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spr-" base-name))))

(defn make-screen-script-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-screenscr-" base-name))))

(defn make-sprite-script-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spritescr-" base-name))))

(defn make-title-pattern-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-titlepat-" base-name))))

(defn make-title-color-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-titlecol-" base-name))))


;; files

(defn- list-files
  [path extension]
  (->> path
       io/file
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (str/lower-case %) extension))))

(defn list-sprites-files
  []
  (list-files "resources/sprites" ".png"))

(defn list-screen-scripts-files
  []
  (list-files "resources/scripts/screens" ".scr"))

(defn list-sprite-scripts-files
  []
  (list-files "resources/scripts/sprites" ".scr"))

(defn list-title-files
  []
  (list-files "resources/titles" ".png"))


;; sprites

(defn- compile-sprite
  [f]
  (let [name (.getName f)
        id   (make-sprite-id name)]
    (convert-sprite-16x16 f)))

(defn- make-sprites
  []
  (->> (list-sprites-files)
       (map (fn [file]
              (let [name   (.getName file)
                    id     (make-sprite-id name)
                    sprite (compile-sprite file)]
                (println "Compiled sprite" name
                         (count sprite) "bytes")
                (make-proc id 3 [(apply db sprite)]))))
       dorun))


;; scripts

(defn- compile-screen-scripts
  []
  (->> (list-screen-scripts-files)
       (map (fn [file]
              (let [name   (.getName file)
                    id     (make-screen-script-id name)
                    script (sc/compile-screen-script (slurp file))]
                (println "Compiled screen script" name
                         (apply + (map count (vals script))) "opcodes")
                [id script])))
       (into {})))

(defn- compile-sprite-scripts
  []
  (->> (list-sprite-scripts-files)
       (map (fn [file]
              (let [name   (.getName file)
                    id     (make-sprite-script-id name)
                    script (sc/compile-sprite-script (slurp file))]
                (println "Compiled sprite script" name
                         (apply + (map count (vals script))) "opcodes")
                [id script])))
       (into {})))

(defn- make-scripts
  [scripts]
  (dorun
   (map (fn [[res-id script]]
          (let [non-script [[:ret]]
                init-id    (keyword (str (name res-id) "-init"))
                update-id  (keyword (str (name res-id) "-update"))
                init-asm   (get-in scripts [res-id :init])
                update-asm (get-in scripts [res-id :update])]
            (make-proc init-id 2 (or init-asm non-script))
            (make-proc update-id 2 (or update-asm non-script))))
        scripts)))


;; titles

(defn- compile-title
  [f]
  (let [name              (.getName f)
        [colors patterns] (convert-screen2 f :colors)
        patterns          (compress-lz77 patterns)
        colors            (compress-lz77 colors)]
    [patterns colors]))

(defn- make-titles
  []
  (->> (list-title-files)
       (map (fn [file]
              (let [name              (.getName file)
                    [patterns colors] (compile-title file)]
                (println "Compiled title" name
                         (+ (count patterns)
                            (count colors))
                         "bytes")
                (make-proc (make-title-pattern-id name) 3
                           [(apply db patterns)])
                (make-proc (make-title-color-id name) 3
                           [(apply db colors)]))))
       dorun))


;; core

(defn compile-resources
  []
  (make-sprites)
  (make-titles)
  (make-scripts (compile-screen-scripts))
  (make-scripts (compile-sprite-scripts)))
