(ns mge.script-ir
  (:require [mge.sprites :as spr]
            [mge.keys :as keys]
            [mge.title :as title]
            [clj-z80.asm :refer :all :refer-macros :all]
            [clojure.string :as str]))


;; util

(defn- get-var-index
  [id]
  (case id
    "type"   spr/+spr-type+
    "x"      spr/+spr-x+
    "y"      spr/+spr-y+
    "color"  spr/+spr-color+
    "width"  spr/+spr-w+
    "height" spr/+spr-h+))

(defn- var-source
  [id]
  (cond (string? id) [:ix (get-var-index id)]
        (number? id) id))

(defn- compare-code
  [i j cmp skip-label]
  (concat (case cmp
            "<=" [[:ld :a (var-source j)]
                  [:cp (var-source i)]]
            [[:ld :a (var-source i)]
             [:cp (var-source j)]])
          (case cmp
            "="  [:jp :nz skip-label]
            "<>" [:jp :z skip-label]
            ">"  [[:jp :z skip-label]
                  [:jp :c skip-label]]
            ">=" [[:jp :c skip-label]]
            "<"  [[:jp :nc skip-label]]
            "<=" [[:jp :c skip-label]])))


;; core

(defn end
  []
  [[:ret]])

(defn new-sprite
  [init-id update-id]
  [[:ld :hl init-id]
   [:ld :de update-id]
   [:call spr/new-sprite]])

(defn sprite-delete
  []
  [[:call spr/delete-sprite]])

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

(defn sprite-type
  [n]
  [[:ld [:ix spr/+spr-type+] n]])

(defn sprite-width
  [n]
  [[:ld [:ix spr/+spr-w+] n]])

(defn sprite-height
  [n]
  [[:ld [:ix spr/+spr-h+] n]])

(defn if-keydown
  ([keyname then]
   (let [keyname (str/trim (str/upper-case keyname))
         endif   (keyword (gensym))]
     (concat [(keys/key-down? keyname)
              [:jp :nz endif]]
             then
             [(label endif)])))
  ([keyname then else]
   (let [keyname (str/trim (str/upper-case keyname))
         lelse   (keyword (gensym))
         lendif  (keyword (gensym))]
     (concat [(keys/key-down? keyname)
              [:jp :nz lelse]]
             then
             [[:jp lendif]
              (label lelse)]
             else
             [(label lendif)]))))

(defn if-keypressed
  ([keyname then]
   (let [keyname (str/trim (str/upper-case keyname))
         row     (:row (keys/key-codes keyname))
         bit     (:bit (keys/key-codes keyname))
         endif   (keyword (gensym))]
     (concat [[:ld :e row]
              [:ld :c bit]
              [:call keys/key-pressed?]
              [:jp :z endif]]
             then
             [(label endif)])))
  ([keyname then else]
   (let [keyname (str/trim (str/upper-case keyname))
         row     (:row (keys/key-codes keyname))
         bit     (:bit (keys/key-codes keyname))
         lelse   (keyword (gensym))
         lendif  (keyword (gensym))]
     (concat [[:ld :e row]
              [:ld :c bit]
              [:call keys/key-pressed?]
              [:jp :z lelse]]
             then
             [[:jp lendif]
              (label lelse)]
             else
             [(label lendif)]))))

(defn if-cmp
  ([id cmp num then]
   (let [lendif (keyword (gensym))]
     (concat (compare-code id num cmp lendif)
             then
             [(label lendif)])))
  ([id cmp num then else]
   (let [lelse  (keyword (gensym))
         lendif (keyword (gensym))]
     (concat (compare-code id num cmp lelse)
             then
             [[:jp lendif]
              (label lelse)]
             else
             [(label lendif)]))))

(defn if-collide
  ([type then]
   [[:nop]]
   (let [lendif (keyword (gensym))]
     (concat [[:ld :a type]
              [:call spr/collide]
              [:jp :z lendif]]
             then
             [(label lendif)])))
  ([type then else]
   (let [lelse  (keyword (gensym))
         lendif (keyword (gensym))]
     (concat [[:ld :a type]
              [:call spr/collide]
              [:jp :z lelse]]
             then
             [[:jp lendif]
              (label lelse)]
             else
             [(label lendif)]))))

(defn load-title
  [patterns-id colors-id]
  [[:ld :hl patterns-id]
   [:ld :de colors-id]
   [:call title/load-title]])
