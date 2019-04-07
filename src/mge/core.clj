(ns mge.core
  (:gen-class)
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]]
            [mge.resources :refer [compile-resources]]
            [mge.sprites :as spr]
            [mge.screens :as scr]
            [mge.keys :as keys]
            [mge.music :as music]
            [mge.tilemap :as tilemap]
            [mge.offscreen :as off]
            mge.image))

(defasmbyte skip-frame)

(defasmproc init {:page :code}
  ;; vars
  [:xor :a]
  [:ld [skip-frame] :a]
  ;; screen 2,2
  [:ld [sysvars/FORCLR] :a]
  [:ld [sysvars/BAKCLR] :a]
  [:ld [sysvars/BDRCLR] :a]
  [:ld :a 2]
  [:call bios/CHGMOD]
  [:call music/init-music]
  [:call off/init-offscreen]
  [:call spr/init-sprites]
  [:call scr/init-screens]
  [:jp keys/init-keys])

(defasmproc main-loop {:page :code}
  [:push :af]
  ;; save vdp status
  [:ld [spr/vdps0] :a]
  ;; music
  [:call music/update-music]
  ;; check skip frame
  [:ld :a [skip-frame]]
  [:or :a]
  [:jp :nz :end]
  [:inc :a]
  [:ld [skip-frame] :a]
  ;; main loop
  [:call off/update-offscreen]
  [:call scr/update-screens]
  [:call spr/update-sprites]
  [:call keys/update-keys]
  ;; reset skip frame
  [:xor :a]
  [:ld [skip-frame] :a]
  (label :end
         [:pop :af]
         [:ret]))

(defasmproc install-hook {:page :code}
  [:ld :hl :data]
  [:ld :de sysvars/H_TIMI]
  [:ld :bc 3]
  [:ldir]
  [:ret]
  (label :data [:jp main-loop]))

(defasmproc entry {:page 0 :include-always true :label :entry}
  [:ei]
  [:call init]
  [:call install-hook]
  (label :loop [:jr :loop]))

(defn- build
  [{:keys [sym name run-openmsx asm-code] :or {name "game"}}]
  (let [rom-file (str name ".rom")
        sym-file (str name ".sym")
        asm-file (when asm-code (str name ".asm"))]
    (compile-resources asm-file)
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
