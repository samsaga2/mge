(ns mge.music
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [clj-z80.image :refer [get-label]]))

(def replayer-address 0xc000)
(def ayfx-setup (+ replayer-address 0))
(def ayfx-end   (+ replayer-address 3))
(def ayfx-init  (+ replayer-address 6))
(def ayfx-play  (+ replayer-address 9))
(def pt3-init   (+ replayer-address 12))
(def pt3-rout   (+ replayer-address 15))
(def pt3-play   (+ replayer-address 18))
(def pt3-mute   (+ replayer-address 21))

(defasmbyte music-page)
(defasmbyte sfx-page)

(let [replayer-bin (incbin "resources/pt3.bin")]
  (def replayer-size (-> replayer-bin second count))
  (defasmproc replayer {}
    replayer-bin))

(defasmproc init-music {:page 0}
  ;; move replayer to RAM
  (set-konami5-page 3 (fn [] (:page (get-label (replayer)))))
  [:ld :hl replayer]
  [:ld :de replayer-address]
  [:ld :bc replayer-size]
  [:inc :bc]
  [:ldir]
  ;; init
  [:xor :a]
  [:ld [music-page] :a]
  [:call pt3-mute]
  [:jp ayfx-end])

(defasmproc update-music {:page :code}
  ;; write psg regs
  [:call pt3-rout]
  ;; music
  [:ld :a [music-page]]
  (set-konami5-page 3 :a)
  [:call pt3-play]
  ;; sfx
  (label :sfx
         [:ld :a [sfx-page]]
         (set-konami5-page 3 :a)
         [:jp ayfx-play])
  [:ret])

(defasmproc play-music {:page :code}
  ;; in a=page hl=address
  [:ld :de -100]
  [:add :hl :de]
  [:ld [music-page] :a]
  (set-konami5-page 3 :a)
  [:jp pt3-init])

(defasmproc stop-music {:page :code}
  [:xor :a]
  [:ld [music-page] :a]
  [:jp pt3-mute])

(defasmproc load-sfx {:page :code}
  ;; in a=page hl=address
  [:ld [sfx-page] :a]
  (set-konami5-page 3 :a)
  [:jp ayfx-setup])

(defasmproc play-sfx {:page :code}
  ;; in a=sfx c=priority
  [:push :af]
  [:ld :a [sfx-page]]
  (set-konami5-page 3 :a)
  [:pop :af]
  [:jp ayfx-init])
