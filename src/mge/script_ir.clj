(ns mge.script-ir
  (:require [mge.sprites :as spr]
            [mge.keys :as keys]
            [mge.title :as title]
            [clj-z80.asm :refer :all :refer-macros :all]
            [clojure.string :as str]
            [mge.script :as s]
            [mge.util :as u]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [clj-z80.image :refer [get-label]]
            [mge.music :as music]
            [mge.tilemap :as tilemap]))


;; args

(defn- get-localvar-index
  [id]
  (case id
    "type"    spr/+spr-type+
    "x"       spr/+spr-x+
    "y"       spr/+spr-y+
    "width"   spr/+spr-w+
    "height"  spr/+spr-h+
    "local0"  spr/+spr-local0+
    "local1"  (+ spr/+spr-local0+ 1)
    "local2"  (+ spr/+spr-local0+ 2)
    "local3"  (+ spr/+spr-local0+ 3)
    "local4"  (+ spr/+spr-local0+ 4)
    "local5"  (+ spr/+spr-local0+ 5)
    "local6"  (+ spr/+spr-local0+ 6)
    "local7"  (+ spr/+spr-local0+ 7)
    "local8"  (+ spr/+spr-local0+ 8)
    "local9"  (+ spr/+spr-local0+ 9)
    "local10" (+ spr/+spr-local0+ 10)
    "local11" (+ spr/+spr-local0+ 11)
    "local12" (+ spr/+spr-local0+ 12)
    "local13" (+ spr/+spr-local0+ 13)
    "local14" (+ spr/+spr-local0+ 14)
    "local15" (+ spr/+spr-local0+ 15)
    nil))

(let [vars (->> (concat s/args s/globals)
                   (map (fn [g] [(second (str/split (name g) #"---")) g]))
                   (into {}))]
  (defn- get-globalvar-addr
    [id]
    (get vars id)))

(defn- var-source
  [id]
  (cond (string? id) (if-let [i (get-localvar-index id)]
                       [:ix i]
                       (if-let [addr (get-globalvar-addr id)]
                         [addr]
                         (throw (Exception. "Uknown variable " id))))
        (number? id) id))

(defn- arg-source
  [arg]
  (let [v (second arg)]
    (case (first arg)
      :num (var-source (Integer. v))
      :id  (var-source v))))

(defn- load-arg
  [arg & [reg]]
  (if (and (= (first arg) :id)
           (= (str/lower-case (second arg)) "rnd"))
    [[:call u/random-word]
     [:ld (or reg :a) :l]]
    [[:ld (or reg :a) (arg-source arg)]]))

(defn- store-arg
  [arg & [reg]]
  [[:ld (arg-source arg) (or reg :a)]])


;; util

(defn- compare-code
  [i j cmp skip-label]
  (concat (case cmp
            "<=" [(load-arg j)
                  (load-arg i :b)
                  [:cp :b]]
            [(load-arg i)
             (load-arg j :b)
             [:cp :b]])
          (case cmp
            "="  [[:jp :nz skip-label]]
            "<>" [[:jp :z skip-label]]
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

(defn- push-args
  [args]
  (when (> (count args) (count s/args))
    (throw (Exception. "Too many args for new sprite")))
  (mapcat (fn [argvar arg]
            [[:ld :hl argvar]
             [:ld :a [:hl]]
             [:push :af]
             (load-arg arg)
             [:ld [:hl] :a]])
          s/args
          args))

(defn- pop-args
  [args]
  (mapcat (fn [argvar _]
            [[:pop :af]
             [:ld [argvar] :a]])
          s/args
          args))


;; core

(defn end
  []
  [[:ret]])

(defn new-sprite
  [init-id update-id args]
  (when (> (count args) (count s/args))
    (throw (Exception. "Too many args for new sprite")))
  (concat (push-args args)
          [[:ld :hl init-id]
           [:ld :de update-id]
           [:call spr/new-sprite]]
          (pop-args args)))

(defn sprite-delete
  []
  [[:call spr/delete-sprite]])

(defn sprite-image
  [res-id color1-id color2-id]
  [[:ld [:ix spr/+spr-color1+] color1-id]
   [:ld [:ix spr/+spr-color2+] color2-id]
   (set-konami5-page 3 (fn [] (:page (get-label res-id))))
   [:ld :hl res-id]
   [:call spr/write-pattern]])

(defn sprite-animation
  [res-id]
  [[:ld :hl res-id]
   [:ld [:ix spr/+spr-anim+] :l]
   [:ld [:ix (inc spr/+spr-anim+)] :h]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:ld [:ix (inc spr/+spr-anim-page+)] :h]])

(defn sprite-pos
  [x y]
  [(load-arg x)
   [:ld [:ix spr/+spr-x+] :a]
   (load-arg y)
   [:ld [:ix spr/+spr-y+] :a]])

(defn sprite-move
  [x y]
  [[:ld :a [:ix spr/+spr-x+]]
   (load-arg x :b)
   [:add :b]
   [:ld [:ix spr/+spr-x+] :a]

   [:ld :a [:ix spr/+spr-y+]]
   (load-arg y :b)
   [:add :b]
   [:ld [:ix spr/+spr-y+] :a]])

(defn sprite-type
  [n]
  [(load-arg n)
   [:ld [:ix spr/+spr-type+] :a]])

(defn sprite-width
  [n]
  [(load-arg n)
   [:ld [:ix spr/+spr-w+] :a]])

(defn sprite-height
  [n]
  [(load-arg n)
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
                [:jp :nz l]]))
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
             [(load-arg type)
              [:call spr/collide]
              [:jp :z l]])
           then else)))

(defn load-title
  [patterns-id colors-id]
  [[:di]
   (set-konami5-page 3 (fn [] (:page (get-label patterns-id))))
   [:ld :hl patterns-id]
   [:call title/load-patterns]
   (set-konami5-page 3 (fn [] (:page (get-label colors-id))))
   [:ld :hl colors-id]
   [:call title/load-colors]
   [:ei]])

(defn return
  []
  [[:ret]])

(defn assign-val
  [id n]
  [(load-arg n)
   (store-arg id)])

(defn assign-add
  [id arg1 arg2]
  [(load-arg arg2)
   [:ld :b :a]
   (load-arg arg1)
   [:add :b]
   (store-arg id)])

(defn assign-sub
  [id arg1 arg2]
  [(load-arg arg2)
   [:ld :b :a]
   (load-arg arg1)
   [:sub :b]
   [:ld (arg-source id) :a]])

(defn call
  [func args]
  (concat (push-args args)
          [[:call func]]
          (pop-args args)))

(defn animation-end
  []
  [[:ld [:ix spr/+spr-anim+] 0]
   [:ld [:ix (inc spr/+spr-anim+)] 0]
   [:ret]])

(defn animation-next-frame
  []
  [[:call spr/animation-next-frame]])

(defn animation-load
  [res-id]
  [[:ld :hl res-id]
   [:ld [:ix spr/+spr-anim+] :l]
   [:ld [:ix (inc spr/+spr-anim+)] :h]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:ld [:ix (inc spr/+spr-anim-page+)] :a]
   [:ret]])

(defn music-load
  [res-id]
  [[:ld :hl res-id]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:call music/play-music]])

(defn music-stop
  [res-id]
  [[:call music/stop-music]])

(defn sfx-load
  [res-id]
  [[:ld :hl res-id]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:call music/load-sfx]])

(defn sfx-play
  [n]
  [(load-arg n)
   [:ld :c 0]
   [:call music/play-sfx]])

(defn tilemap-load
  [patterns-id colors-id attrs-id lines-id map-id]
  [[:di]
   ;; name
   [:call tilemap/clear-name]
   ;; patterns
   (set-konami5-page 3 (fn [] (:page (get-label patterns-id))))
   [:ld :hl patterns-id]
   [:call tilemap/load-patterns]
   ;; colors
   (set-konami5-page 3 (fn [] (:page (get-label colors-id))))
   [:ld :hl colors-id]
   [:call tilemap/load-colors]
   ;; attrs
   (set-konami5-page 3 (fn [] (:page (get-label attrs-id))))
   [:ld :ix attrs-id]
   [:call tilemap/load-attrs]
   ;; map
   [:ld :a (fn [] (:page (get-label lines-id)))]
   [:ld :b (fn [] (:page (get-label map-id)))]
   [:ld :ix map-id]
   [:call tilemap/load-horizontal-map]
   [:ei]])
