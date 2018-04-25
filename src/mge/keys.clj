(ns mge.keys
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.lib.keys :as keys]
            [clj-z80.msx.lib.bios :as bios]
            [clj-z80.msx.lib.sysvars :as sysvars]))

(def key-codes keys/key-codes)
(def key-down? keys/key-down?)
(def key-pressed? keys/key-pressed?)
(def update-keys keys/update-keys)

(defasmproc init-keys {:page :code}
  [:xor :a]
  [:ld [sysvars/CLIKSW] :a]
  [:jp keys/init-keys])

