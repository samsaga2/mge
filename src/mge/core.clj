(ns mge.core
  (:gen-class)
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clojure.java.shell :refer [sh]]
            [mge.resources :refer [compile-resources]]
            [mge.sprites :as spr]
            [mge.screens :as scr]
            [mge.script :as s]
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
  [:call s/init-scripts]
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

(defn -main
  [& args]
  (compile-resources)
  (build-asm-image-file "game.rom" :mge-konami5)
  (build-sym-file "game.sym")
  (sh "openmsx" "-carta" "game.rom" "-ext" "debugdevice")
  (when *command-line-args*
    (System/exit 0)))

(when-not *command-line-args*
  (-main))
