(ns mge.resources
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.image :refer [set-label!]]
            [clojure.java.io :as io]
            [clj-z80.msx.util.sprites :refer [convert-sprite-16x16]]
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


;; sprites

(defn- compile-sprite
  [f]
  (let [name (.getName f)
        id   (make-sprite-id name)]
    (convert-sprite-16x16 f)))

(defn- compile-sprites
  []
  (->> (list-sprites-files)
       (map (fn [file]
              (let [name (.getName file)
                    id   (make-sprite-id name)]
                [id (compile-sprite file)])))
       (into {})))

(defn- make-sprites
  [sprites]
  (->> sprites
       (map (fn [[res-id sprite]]
              (make-proc res-id 3 [(apply db sprite)])))
       dorun))


;; scripts

(defn- compile-screen-scripts
  []
  (->> (list-screen-scripts-files)
       (map (fn [file]
              (let [name (.getName file)
                    id   (make-screen-script-id name)
                    script (sc/compile-screen-script (slurp file))]
                [id script])))
       (into {})))

(defn- compile-sprite-scripts
  []
  (->> (list-sprite-scripts-files)
       (map (fn [file]
              (let [name (.getName file)
                    id   (make-sprite-script-id name)
                    script (sc/compile-sprite-script (slurp file))]
                [id script])))
       (into {})))

(defn- make-scripts
  [scripts]
  (->> scripts
       (map (fn [[res-id script]]
              (let [non-script [[:ret]]
                    init-id    (keyword (str (name res-id) "-init"))
                    update-id  (keyword (str (name res-id) "-update"))
                    init-asm   (get-in scripts [res-id :init])
                    update-asm (get-in scripts [res-id :update])]
                (make-proc init-id 2 (or init-asm non-script))
                (make-proc update-id 2 (or update-asm non-script)))))
       dorun))


;; core

(defn compile-resources
  []
  (make-sprites (compile-sprites))
  (let [screens (compile-screen-scripts)
        sprites (compile-sprite-scripts)]
    (make-scripts screens)
    (make-scripts sprites)))
