(ns mge.script-ir
  (:require [mge.sprites :as spr]
            [mge.keys :as keys]
            [clj-z80.asm :refer :all :refer-macros :all]
            [clojure.string :as str]))

(defn end
  []
  [[:ret]])

(defn new-sprite
  [init-id update-id]
  [[:ld :hl init-id]
   [:ld :de update-id]
   [:call spr/new-sprite]])

(defn sprite-image
  [res-id]
  [[:ld :hl res-id]
   [:call spr/write-pattern]])

(defn sprite-pos
  [x y]
  [[:ld [:ix spr/+spr-x+] x]
   [:ld [:ix spr/+spr-y+] y]])

(defn sprite-move
  [x y]
  [[:ld :a [:ix spr/+spr-x+]]
   [:add x]
   [:ld [:ix spr/+spr-x+] :a]

   [:ld :a [:ix spr/+spr-y+]]
   [:add y]
   [:ld [:ix spr/+spr-y+] :a]])

(defn sprite-color
  [n]
  [[:ld [:ix spr/+spr-color+] n]])

(defn if-keydown
  ([keyname then]
   (let [keyname (str/trim (str/upper-case keyname))
         endif   (keyword (gensym))]
     (concat [(keys/key-pressed? keyname)
              [:jp :nz endif]]
             then
             [(label endif)])))
  ([keyname then else]
   (let [keyname (str/trim (str/upper-case keyname))
         lelse   (keyword (gensym))
         lendif  (keyword (gensym))]
     (concat [(keys/key-pressed? keyname)
              [:jp :nz lelse]]
             then
             [[:jp lendif]
              (label lelse)]
             else
             [(label lendif)]))))
