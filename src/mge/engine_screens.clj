(ns mge.engine-screens
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [mge.engine-offscreen :as off]
            [mge.engine-sprites :as spr]
            [mge.engine-keys :as keys]
            [mge.engine-util :as u]))


(defasmword update-fn)


;; core

(defasmproc update-screens {:page :code}
  [:ld :hl [update-fn]]
  [:push :hl]
  [:ret])

(defasmproc init-screens {:page :code}
  [:ld :hl :res-screenscr-main-update]
  [:ld [update-fn] :hl]
  [:jp :res-screenscr-main-init])

(defasmproc load-screen {:page :code}
  ;; in hl=init-fn de=update-fn
  [:push :de]
  [:push :hl]
  [:call off/init-offscreen]
  [:call spr/init-sprites]
  [:call keys/init-keys]
  [:call spr/update-attributes]
  [:pop :hl]
  [:call u/call-hl]
  [:pop :hl]
  [:ld [update-fn] :hl]
  [:ret])
