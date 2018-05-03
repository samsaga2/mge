(ns mge.resources
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.image :refer [set-label!]]
            [mge.resources-id :refer :all]
            [clojure.java.io :as io]
            [clj-z80.msx.util.graphics :refer [convert-screen2 convert-sprite-16x16]]
            [clj-z80.msx.util.compress :refer [compress-lz77]]
            [mge.script-compile :as sc]
            [clojure.string :as str]))



(def script-pages [0 1 2])
(def res-pages (vec (range 4 64)))


;; files

(defn- list-files
  [path extension]
  (->> path
       io/file
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (str/lower-case %) extension))
       doall))

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

(defn- make-sprites
  []
  (doseq [file (list-sprites-files)]
    (let [name     (.getName file)
          id       (make-sprite-id name)
          id-color (make-sprite-color-id name)
          sprite   (convert-sprite-16x16 file)
          colors   (:colors sprite)
          patterns (flatten (:patterns sprite))]
      (assert (= (count colors) 1))
      (println "Compiled sprite" name
               (count patterns) "bytes")
      (make-proc id res-pages [(apply db patterns)])
      (set-label! id-color (first colors)))))


;; scripts

(defn- compile-screen-scripts
  []
  (->> (list-screen-scripts-files)
       (map (fn [file]
              (let [name   (.getName file)
                    id     (make-screen-script-id name)
                    script (sc/compile-script file)]
                (println "Compiled screen script" name
                         (apply + (map count (vals script))) "opcodes")
                [id script])))
       (into {})
       doall))

(defn- compile-sprite-scripts
  []
  (->> (list-sprite-scripts-files)
       (map (fn [file]
              (let [name   (.getName file)
                    id     (make-sprite-script-id name)
                    script (sc/compile-script file)]
                (println "Compiled sprite script" name
                         (apply + (map count (vals script))) "opcodes")
                [id script])))
       (into {})
       doall))

(defn- make-scripts
  [scripts]
  (doseq [[res-id script] scripts]
    (let [non-script [[:ret]]
          script     (get scripts res-id)
          script     (merge {:update nil} script)]
      (doseq [[func-name func-asm] script]
        (let [func-id (keyword (str (name res-id) "-" (name func-name)))]
          (make-proc func-id script-pages (or func-asm non-script)))))))


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
  (doseq [file (list-title-files)]
    (let [name              (.getName file)
          [patterns colors] (compile-title file)]
      (println "Compiled title" name
               (+ (count patterns)
                  (count colors))
               "bytes")
      (make-proc (make-title-pattern-id name) res-pages
                 [(apply db patterns)])
      (make-proc (make-title-color-id name) res-pages
                 [(apply db colors)]))))


;; core

(defn compile-resources
  []
  (make-sprites)
  (make-titles)
  (make-scripts (compile-screen-scripts))
  (make-scripts (compile-sprite-scripts)))
