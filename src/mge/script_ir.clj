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

(defn- gen-if
  [condfn then else]
  (if else
    ;; if-then-else
    (let [lelse  (keyword (gensym))
          lendif (keyword (gensym))]
      (concat (condfn lelse)
              then
              [[:jp lendif]]
              (label lelse)
              else
              (label lendif)))
    ;; if-then
    (let [lendif (keyword (gensym))]
      (concat (condfn lendif)
              then
              (label lendif)))))


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
   (if-keydown keyname then nil))
  ([keyname then else]
   (gen-if (fn [l]
             (let [keyname (str/trim (str/upper-case keyname))]
               [(keys/key-down? keyname)
                [:jp :nz l]]))
           then else)))

(defn if-keypressed
  ([keyname then]
   (if-keypressed keyname then nil))
  ([keyname then else]
   (gen-if (fn [l]
             (let [keyname (str/trim (str/upper-case keyname))
                   keycode (keys/key-codes keyname)]
               [[:ld :e (:row keycode)]
                [:ld :c (:bit keycode)]
                [:call keys/key-pressed?]
                [:jp :z l]]))
           then else)))

(defn if-cmp
  ([id cmp num then]
   (if-cmp id cmp num then nil))
  ([id cmp num then else]
   (gen-if (fn [l]
             (compare-code id num cmp l))
           then else)))

(defn if-collide
  ([type then]
   (if-collide type then nil))
  ([type then else]
   (gen-if (fn [l]
             [[:ld :a (arg-source type)]
              [:call spr/collide]
              [:jp :z l]])
           then else)))

(defn load-title
  [patterns-id colors-id]
  [[:ld :hl patterns-id]
   [:ld :de colors-id]
   [:call title/load-title]])

(defn return
  []
  [[:ret]])
