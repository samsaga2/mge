(ns mge.engine-image
  (:require [clj-z80.asm-header :refer [setup-image-header!
                                        variables-origin]]))

(defmethod setup-image-header! :mge-konami5
  [_]
  (setup-image-header! :msx-konami5)
  (let [replayer-size 2100]
    (reset! variables-origin (+ 0xc000 replayer-size))))
