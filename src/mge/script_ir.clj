(ns mge.script-ir
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [clj-z80.image :refer [get-label]]
            [clj-z80.msx.lib.bios :as bios]
            [clojure.string :as str]
            [mge.sprites :as spr]
            [mge.keys :as keys]
            [mge.title :as title]
            [mge.script :as s]
            [mge.util :as u]
            [mge.music :as music]
            [mge.tilemap :as tilemap]
            [mge.screens :as scr]
            [mge.offscreen :as off]))


;; args

(defn- get-byte-localvar-index
  [id]
  (case id
    "type"    spr/+spr-type+
    "x"       spr/+spr-x+
    "y"       spr/+spr-y+
    "width"   spr/+spr-w+
    "height"  spr/+spr-h+
    nil))

(defn- get-word-localvar-index
  [id]
  (case id
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

(defn- load-arg
  [arg]
  (let [type (first arg)
        v    (second arg)]
    (case type
      :id  (if (= (str/lower-case (second arg)) "rnd")
             [[:call u/random-word]]
             (if-let [i (get-byte-localvar-index v)]
               [[:ld :l [:ix i]]
                [:ld :h 0]]
               (if-let [i (get-word-localvar-index v)]
                 [[:ld :l [:ix i]]
                  [:ld :h [:ix (inc i)]]]
                 (if-let [addr (get-globalvar-addr v)]
                   [[:ld :hl [addr]]]
                   (throw (Exception. "Uknown variable " v))))))
      :num [[:ld :hl (Integer. v)]])))

(defn- store-arg
  [arg]
  (let [type (first arg)
        v    (second arg)]
    (case type
      :id  (if-let [i (get-byte-localvar-index v)]
             [[:ld [:ix i] :l]]
             (if-let [i (get-word-localvar-index v)]
               [[:ld [:ix i] :l]
                [:ld [:ix (inc i)] :h]]
               (if-let [addr (get-globalvar-addr v)]
                 [[:ld [addr] :hl]]
                 (throw (Exception. "Uknown variable " v))))))))


;; util

(defn- compare-code
  [i j cmp skip-label]
  (concat (if (= cmp "<=")
            [(load-arg j)
             [:push :hl]
             (load-arg i)
             [:ex :de :hl]
             [:pop :hl]]
            [(load-arg i)
             [:push :hl]
             (load-arg j)
             [:ex :de :hl]
             [:pop :hl]])
          [[:call bios/DCOMPR]]
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
            [[:ld :hl [argvar]]
             [:push :hl]
             (load-arg arg)
             [:ld [argvar] :hl]])
          s/args
          args))

(defn- pop-args
  [args]
  (mapcat (fn [argvar _]
            [[:pop :hl]
             [:ld [argvar] :hl]])
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
   [:ld [:ix (inc spr/+spr-anim-page+)] :a]])

(defn sprite-pos
  [x y]
  [(load-arg x)
   [:ld [:ix spr/+spr-x+] :l]
   (load-arg y)
   [:ld [:ix spr/+spr-y+] :l]])

(defn sprite-move
  [x y]
  [[:ld :a [:ix spr/+spr-x+]]
   (load-arg x)
   [:add :l]
   [:ld [:ix spr/+spr-x+] :a]

   [:ld :a [:ix spr/+spr-y+]]
   (load-arg y)
   [:add :l]
   [:ld [:ix spr/+spr-y+] :a]])

(defn sprite-type
  [n]
  [(load-arg n)
   [:ld [:ix spr/+spr-type+] :l]])

(defn sprite-width
  [n]
  [(load-arg n)
   [:ld [:ix spr/+spr-w+] :l]])

(defn sprite-height
  [n]
  [(load-arg n)
   [:ld [:ix spr/+spr-h+] :l]])

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
              [:ld :a :l]
              [:call spr/collide]
              [:jp :z l]])
           then else)))

(defn if-tile
  ([offset-x offset-y type then]
   (if-tile offset-x offset-y type then nil))
  ([offset-x offset-y type then else]
   (gen-if (fn [l]
             [(load-arg offset-x)
              [:ld :a :l]
              [:ld :b :a]
              (load-arg offset-y)
              [:ld :a :l]
              [:ld :c :a]
              [:call spr/get-tile]
              [:ld :b :a]

              (load-arg type)
              [:ld :a :l]

              [:cp :b]
              [:jp :nz l]])
           then else)))

(defn load-title
  [patterns-id colors-id]
  [[:ld :a (fn [] (:page (get-label patterns-id)))]
   [:ld :hl patterns-id]
   [:ld :b (fn [] (:page (get-label colors-id)))]
   [:ld :de colors-id]
   [:call title/load-title]])

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
   [:ex :de :hl]
   (load-arg arg1)
   [:add :hl :de]
   (store-arg id)])

(defn assign-sub
  [id arg1 arg2]
  [(load-arg arg2)
   [:ex :de :hl]
   (load-arg arg1)
   [:or :a]
   [:sbc :hl :de]
   (store-arg id)])

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
   [:ld :a :l]
   [:ld :c 0]
   [:call music/play-sfx]])

(defn tilemap-load
  [patterns-id colors-id attrs-id lines-id map-id types-id]
  [;; name
   [:call tilemap/clear-name]
   [:di]
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
   [:ld :c (fn [] (:page (get-label types-id)))]
   [:ld :hl types-id]
   [:ld :ix map-id]
   [:call tilemap/load-horizontal-map]
   [:ei]])

(defn scroll-right
  []
  [[:call tilemap/scroll-right]])

(defn scroll-left
  []
  [[:call tilemap/scroll-left]])

(defn screen-load
  [init-id update-id]
  [[:ld :hl init-id]
   [:ld :de update-id]
   [:call scr/load-screen]])

(defn set-tile
  [x y n]
  [(load-arg n)
   [:ld :a :l]
   [:ld :c :a]
   (load-arg y)
   [:ld :a :l]
   [:ld :b :a]
   (load-arg x)
   [:ld :a :l]
   [:call off/set-tile]])

(defn write-str
  [x y str]
  (let [str-label  (keyword (gensym))
        next-label (keyword (gensym))]
    [[:ld :de str-label]
     (load-arg y)
     [:ld :a :l]
     [:ld :b :a]
     (load-arg x)
     [:ld :a :l]
     [:call off/write-print]
     [:jr next-label]
     (label str-label (db str) (db 0))
     (label next-label)]))
