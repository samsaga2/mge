(ns mge.script-ir
  (:require [mge.sprites :as spr]
            [mge.keys :as keys]
            [mge.title :as title]
            [clj-z80.asm :refer :all :refer-macros :all]
            [clojure.string :as str]))


;; script vars

(defasmbyte arg0)
(defasmbyte arg1)
(defasmbyte arg2)
(defasmbyte arg3)


;; util

(defn- get-localvar-index
  [id]
  (case id
    "type"   spr/+spr-type+
    "x"      spr/+spr-x+
    "y"      spr/+spr-y+
    "color"  spr/+spr-color+
    "width"  spr/+spr-w+
    "height" spr/+spr-h+
    nil))

(defn- get-globalvar-addr
  [id]
  (case id
    "arg0"   arg0
    "arg1"   arg1
    "arg2"   arg2
    "arg3"   arg3
    nil))

(defn- var-source
  [id]
  (cond (string? id) (if-let [i (get-localvar-index id)]
                       [:ix i]
                       (if-let [addr (get-globalvar-addr id)]
                         [addr]
                         (throw (Exception. "Uknown variable " id))))
        (number? id) id))

(var-source "arg0")

(defn- arg-source
  [arg]
  (let [v (second arg)]
    (case (first arg)
      :num (var-source (Integer. v))
      :id  (var-source v))))

(defn- compare-code
  [i j cmp skip-label]
  (concat (case cmp
            "<=" [[:ld :a (arg-source j)]
                  [:cp (arg-source i)]]
            [[:ld :a (arg-source i)]
             [:cp (arg-source j)]])
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
  [init-id update-id args]
  (let [argvars [arg0 arg1 arg2 arg3]]
    (when (> (count args) (count argvars))
      (throw (Exception. "Too many args for new sprite")))
    ;; save current args and set the new args
    ;; (this is slow but its simple and this code will not called very often)
    (concat
     (mapcat (fn [argvar arg]
               [[:ld :hl argvar]
                [:ld :a [:hl]]
                [:push :af]
                [:ld :a (arg-source arg)]
                [:ld [:hl] :a]])
             argvars
             args)
     ;; create new sprite
     [[:ld :hl init-id]
      [:ld :de update-id]
      [:call spr/new-sprite]]
     ;; restore args
     (mapcat (fn [argvar _]
               [[:pop :af]
                [:ld [argvar] :a]])
             argvars
             args))))

(defn sprite-delete
  []
  [[:call spr/delete-sprite]])

(defn sprite-image
  [res-id]
  [[:ld :hl res-id]
   [:call spr/write-pattern]])

(defn sprite-pos
  [x y]
  [[:ld :a (arg-source x)]
   [:ld [:ix spr/+spr-x+] :a]
   [:ld :a (arg-source y)]
   [:ld [:ix spr/+spr-y+] :a]])

(defn sprite-move
  [x y]
  [[:ld :a [:ix spr/+spr-x+]]
   [:add (arg-source x)]
   [:ld [:ix spr/+spr-x+] :a]

   [:ld :a [:ix spr/+spr-y+]]
   [:add (arg-source y)]
   [:ld [:ix spr/+spr-y+] :a]])

(defn sprite-color
  [n]
  [[:ld :a (arg-source n)]
   [:ld [:ix spr/+spr-color+] :a]])

(defn sprite-type
  [n]
  [[:ld :a (arg-source n)]
   [:ld [:ix spr/+spr-type+] :a]])

(defn sprite-width
  [n]
  [[:ld :a (arg-source n)]
   [:ld [:ix spr/+spr-w+] :a]])

(defn sprite-height
  [n]
  [[:ld :a (arg-source n)]
   [:ld [:ix spr/+spr-h+] :a]])

(defn if-keydown
  ([keyname then]
   (let [keyname (str/trim (str/upper-case keyname))
         endif   (keyword (gensym))]
     (concat [(keys/key-down? keyname)
              [:jp :nz endif]]
             then
             (label endif))))
  ([keyname then else]
   (let [keyname (str/trim (str/upper-case keyname))
         lelse   (keyword (gensym))
         lendif  (keyword (gensym))]
     (concat [(keys/key-down? keyname)
              [:jp :nz lelse]]
             then
             [[:jp lendif]]
             (label lelse)
             else
             (label lendif)))))

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
             (label endif))))
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
             [[:jp lendif]]
             (label lelse)
             else
             (label lendif)))))

(defn if-cmp
  ([id cmp num then]
   (let [lendif (keyword (gensym))]
     (concat (compare-code id num cmp lendif)
             then
             (label lendif))))
  ([id cmp num then else]
   (let [lelse  (keyword (gensym))
         lendif (keyword (gensym))]
     (concat (compare-code id num cmp lelse)
             then
             [[:jp lendif]]
             (label lelse)
             else
             (label lendif)))))

(defn if-collide
  ([type then]
   [[:nop]]
   (let [lendif (keyword (gensym))]
     (concat [[:ld :a (arg-source type)]
              [:call spr/collide]
              [:jp :z lendif]]
             then
             (label lendif))))
  ([type then else]
   (let [lelse  (keyword (gensym))
         lendif (keyword (gensym))]
     (concat [[:ld :a (arg-source type)]
              [:call spr/collide]
              [:jp :z lelse]]
             then
             [[:jp lendif]]
             (label lelse)
             else
             (label lendif)))))

(defn load-title
  [patterns-id colors-id]
  [[:ld :hl patterns-id]
   [:ld :de colors-id]
   [:call title/load-title]])

(defn return
  []
  [[:ret]])
