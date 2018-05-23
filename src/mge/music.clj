(ns mge.music
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [clj-z80.image :refer [get-label]]))

(def replayer-address 0xc000)
(def pt3-init (+ replayer-address 3))
(def pt3-play (+ replayer-address 5))
(def pt3-mute (+ replayer-address 8))

(defasmbyte music-page)

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
  [:jp pt3-mute])

(defasmproc update-music {:page :code}
  [:ld :a [music-page]]
  (set-konami5-page 3 :a)
  [:jp pt3-play])

(defasmproc play-music {:page :code}
  ;; in a=page hl=address
  [:ld [music-page] :a]
  [:jp pt3-init])

(defasmproc stop-music {:page :code}
  [:jp pt3-mute])
