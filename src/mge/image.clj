(ns mge.image
  (:require [clj-z80.asm-header :refer [setup-image-header! variables-origin]]))

(defmethod setup-image-header! :mge-konami5
  [_]
  (setup-image-header! :msx-konami5)
  (let [pt3-replayer-size 0x900]
    (reset! variables-origin (+ 0xc000 pt3-replayer-size))))
