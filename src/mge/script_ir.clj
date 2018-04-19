(ns mge.script-ir
  (:require [mge.sprites :as spr]))

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
