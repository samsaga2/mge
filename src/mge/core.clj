(ns mge.core
  (:gen-class)
  (:require [clj-z80.asm :refer [build-asm-image-file build-sym-file]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]]
            [mge.resources :refer [compile-resources]]
            mge.engine-core
            mge.engine-image))

(defn- build
  [{:keys [chdir sym name run-openmsx asm-code]
    :or {name "game" chdir "game"}}]
  (let [rom-file (str name ".rom")
        sym-file (str name ".sym")
        asm-file (when asm-code (str name ".asm"))]
    (compile-resources asm-file chdir)
    (build-asm-image-file rom-file :mge-konami5)
    (when sym
      (build-sym-file sym-file))
    (when run-openmsx
      (sh "openmsx" "-carta" rom-file "-ext" "debugdevice"))))

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
    (build (:options args)))
  (System/exit 0))
