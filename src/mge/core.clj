(ns mge.core
  (:gen-class)
  (:require [clj-z80.asm :refer [build-asm-image-file build-sym-file]]
            [clj-z80.image :refer [get-label]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]]
            [mge.resources :refer [compile-resources]]
            [clojure.java.io :as io]
            mge.engine-core
            mge.engine-image))

(defn- assert-directory
  [dir-name]
  (let [dir (io/file dir-name)]
    (when-not (and (.exists dir)
                   (.isDirectory dir))
      (throw (Exception. (str "Game directory `" dir-name "' does not exists or is not a directory")))))) 

(defn- assert-main-screen
  []
  (try (get-label :res-screenscr-main-init)
       (catch Exception e
         (throw (Exception. (str "Missing script res/scripts/screens/main.mge"))))))
    
(defn- run-emulator
  [rom-file]
  (sh "openmsx" "-carta" rom-file "-ext" "debugdevice"))

(defn- build-game
  [{:keys [chdir sym name run-openmsx asm-code]
    :or {name "game" chdir "game"}}]
  ;; check game folder
  (assert-directory chdir)
  ;; compile resources
  (let [asm-file (when asm-code (str name ".asm"))]
    (compile-resources asm-file chdir))
  ;; assert that the main screen is created
  (let [rom-file (str name ".rom")]
    ;; create game rom
    (build-asm-image-file rom-file :mge-konami5)
    (assert-main-screen)
    ;; create debug sym file
    (when sym
      (let [sym-file (str name ".sym")]
        (build-sym-file sym-file)))
    ;; run the game on the emulator
    (when run-openmsx
      (run-emulator rom-file))))

(def mge-options
  [["-s" "--sym" "Generate sym file"]
   ["-c" "--chdir DIR" "Game folder to compile"
    :default nil]
   ["-n" "--name GAME" "Change the output filename"
    :default "game"]
   ["-r" "--run-openmsx" "Execute openmsx after compile"]
   ["-a" "--asm-code" "Generate pseudo-asm file"]])

(defn -main
  [& args]
  (let [args (parse-opts args mge-options)]
    (try (build-game (:options args))
         (System/exit 0)
         (catch Exception e
           (println "ERROR:" (.getMessage e))
           (System/exit 1)))))
