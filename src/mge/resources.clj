(ns mge.resources
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.image :refer [set-label!]]
            [clj-z80.msx.util.compress :refer [compress-lz77]]
            [mge.graphics-screen2 :refer [convert-screen2]]
            [mge.graphics-sprites :refer [convert-sprite-16x16]]
            [mge.resources-tilemaps :refer [make-tilemap]]
            [mge.script-compile :as sc]
            [mge.resources-id :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:import [java.io File FileInputStream]))


(def script-pages [0 1 2])
(def res-pages (vec (range 4 64)))


;; files

(defn- list-files
  [working-dir path extension]
  (->> (io/file working-dir path)
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (str/lower-case %) extension))
       doall))

(defn collect-resources
  [game-folder]
  {:sprites           (list-files game-folder "res/sprites" ".png")
   :screen-scripts    (list-files game-folder "scripts/screens" ".mge")
   :sprite-scripts    (list-files game-folder "scripts/sprites" ".mge")
   :animation-scripts (list-files game-folder "scripts/animations" ".mge")
   :titles            (list-files game-folder "res/titles" ".png")
   :musics            (list-files game-folder "res/music" ".pt3")
   :sfx               (list-files game-folder "res/sfx" ".afb")
   :tilemaps          (list-files game-folder "res/tilemaps" ".tmx")})


;; sprites

(defn- make-sprites
  [files]
  (doseq [file files]
    (let [name (.getName file)]
      (print "Compiling sprite" name "")
      (flush)
      (let [id        (make-sprite-id name)
            id-color1 (make-sprite-color1-id name)
            id-color2 (make-sprite-color2-id name)
            sprite    (convert-sprite-16x16 file)
            colors    (:colors sprite)
            patterns  (flatten (:patterns sprite))]
        (assert (<= (count colors) 2))
        (println (count patterns) "bytes")
        (make-proc id res-pages [(apply db patterns)])
        (set-label! id-color1 (first colors))
        (set-label! id-color2 (or (second colors) 0))))))


;; scripts

(defn- compile-screen-scripts
  [files resources]
  (->> files
       (map (fn [file]
              (let [name (.getName file)]
                (print "Compiling screen script" name "")
                (flush)
                (let [id     (make-screen-script-id name)
                      script (sc/compile-script file resources)]
                  (println (apply + (map count (vals script))) "opcodes")
                  [id script]))))
       (into {})
       doall))

(defn- compile-sprite-scripts
  [files resources]
  (->> files
       (map (fn [file]
              (let [name (.getName file)]
                (print "Compiling sprite script" name "")
                (flush)
                (let [id     (make-sprite-script-id name)
                      script (sc/compile-script file resources)]
                  (println (apply + (map count (vals script))) "opcodes")
                  [id script]))))
       (into {})
       doall))

(defn- compile-animation-scripts
  [files resources]
  (->> files
       (map (fn [file]
              (let [name (.getName file)]
                (print "Compiling animation script" name "")
                (flush)
                (let [id     (make-animation-script-id name)
                      script (sc/compile-animation file resources)]
                  (println (apply + (map count script)) "opcodes")
                  [id {:update script}]))))
       (into {})
       doall))

(defn- write-asm-code
  [asm-file func-id func-asm]
  (when asm-file
    (let [asm-code (with-out-str
                     (println "FUNCTION" func-id)
                     (pp/pprint func-asm)
                     (println)
                     (println))]
      (spit asm-file asm-code :append true))))

(defn- make-scripts
  [scripts asm-file]
  (doseq [[res-id script] scripts]
    (let [non-script [[:ret]]
          script     (get scripts res-id)
          script     (merge {:update nil} script)]
      (doseq [[func-name func-asm] script]
        (let [func-id  (keyword (str (name res-id) "-" (name func-name)))
              func-asm (or func-asm non-script)]
          (write-asm-code asm-file func-id func-asm)
          (make-proc func-id script-pages func-asm))))))


;; titles

(defn- compile-title
  [f]
  (let [name              (.getName f)
        [colors patterns] (convert-screen2 f :colors)
        patterns          (compress-lz77 patterns)
        colors            (compress-lz77 colors)]
    [patterns colors]))

(defn- make-titles
  [files]
  (doseq [file files]
    (let [name (.getName file)]
      (print "Compiling title" name "")
      (flush)
      (let [[patterns colors] (compile-title file)]
        (println (+ (count patterns) (count colors)) "bytes")
        (make-proc (make-title-pattern-id name) res-pages
                   [[:db patterns]])
        (make-proc (make-title-color-id name) res-pages
                   [[:db colors]])))))

;; music

(defn- make-music
  [files]
  (doseq [file files]
    (let [name (.getName file)]
      (print "Compiling music" name "")
      (flush)
      (let [data (let [arr (byte-array (.length file))]
                   (doto (FileInputStream. file)
                     (.skip 100)
                     (.read arr)
                     (.close))
                   (vec arr))]
        (println (count data) "bytes")
        (make-proc (make-music-id name) res-pages
                   [[:db data]])))))


;; sfx

(defn- make-sfx
  [files]
  (doseq [file files]
    (let [name (.getName file)]
      (print "Compiling sfx" name "")
      (flush)
      (let [data (let [arr (byte-array (.length file))]
                   (doto (FileInputStream. file)
                     (.read arr)
                     (.close))
                   (vec arr))]
        (println (count data) "bytes")
        (make-proc (make-sfx-id name) res-pages
                   [[:db data]])))))


;; tilemaps

(defn- make-tilemaps
  [files]
  (doseq [file files]
    (make-tilemap file)))


;; core

(defn compile-resources
  [asm-file game-folder]
  ;; truncate asm file
  (when asm-file
    (spit asm-file ""))
  (let [resources (collect-resources game-folder)]
    ;; convert resources
    (-> :sprites resources make-sprites)
    (-> :titles resources make-titles)
    (-> :musics resources make-music)
    (-> :sfx resources make-sfx)
    (-> :tilemaps resources make-tilemaps)
    ;; compile scripts
    (-> :screen-scripts
        resources
        (compile-screen-scripts resources)
        (make-scripts asm-file))
    (-> :sprite-scripts
        resources
        (compile-sprite-scripts resources)
        (make-scripts asm-file))
    (-> :animation-scripts
        resources
        (compile-animation-scripts resources)
        (make-scripts asm-file))))
