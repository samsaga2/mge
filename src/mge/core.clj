(ns mge.core
  (:gen-class)
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]
            [clojure.java.shell :refer [sh]]
            [mge.resources :refer [compile-resources]]
            [mge.sprites :as spr]
            [mge.screens :as scr]
            clj-z80.msx.image))

(defasmproc init {:page :code}
  ;; screen 2,2
  [:xor :a]
  [:ld [sysvars/FORCLR] :a]
  [:ld [sysvars/BAKCLR] :a]
  [:ld [sysvars/BDRCLR] :a]
  [:ld :a 2]
  [:call bios/CHGMOD]
  [:call spr/init-sprites]
  [:jp scr/init-screens])

(defasmproc main-loop {:page :code}
  (label :loop
         [:call scr/update-screens]
         [:call spr/update-sprites]
         [:halt]
         [:jr :loop]))

(defasmproc entry {:page 0 :include-always true :label :entry}
  [:ei]
  [:call init]
  [:jp main-loop])

(defn -main
  [& args]
  (compile-resources)
  (build-asm-image-file "test.rom" :msx-konami5)
  (build-sym-file "test.sym"))

(-main)
(sh "openmsx" "-carta" "test.rom")
