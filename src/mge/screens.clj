(ns mge.screens
  (:require [clj-z80.asm :refer :all :refer-macros :all]))


;; core

(defasmproc init-screens {:page :code}
  [:jp :res-screenscr-main-init])

(defasmproc update-screens {:page :code}
  [:jp :res-screenscr-main-update])
