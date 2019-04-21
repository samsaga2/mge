(ns mge.script-gencode
  (:require [clj-z80.asm :refer :all :refer-macros :all]
            [clj-z80.msx.image :refer [set-konami5-page]]
            [clj-z80.image :refer [get-label]]
            [clj-z80.msx.lib.bios :as bios]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [mge.script-gencode-args :refer :all]
            [mge.engine-sprites :as spr]
            [mge.engine-keys :as keys]
            [mge.engine-title :as title]
            [mge.engine-script :as s]
            [mge.engine-util :as u]
            [mge.engine-math :as m]
            [mge.engine-music :as music]
            [mge.engine-tilemap :as tilemap]
            [mge.engine-hscroll :as hscr]
            [mge.engine-screens :as scr]
            [mge.engine-offscreen :as off]
            [mge.script-env :as env]))


(defn- compare-code
  [env i j cmp skip-label]
  (concat 
   (load-arg env i)
   [[:push :hl]]
   (load-arg env j)
   [[:ex :de :hl]
    [:pop :hl]]
   (case cmp
     "="  [[:rst bios/DCOMPR]
           [:jp :nz skip-label]]
     "<>" [[:rst bios/DCOMPR]
           [:jp :z skip-label]]
     ">=" [[:call u/signed-dcompr]
           [:jp :c skip-label]]
     "<"  [[:call u/signed-dcompr]
           [:jp :nc skip-label]]
     ">" [[:rst bios/DCOMPR]
          [:jp :z skip-label]
          [:call u/signed-dcompr]
          [:jp :c skip-label]]
     "<=" (let [no-skip-label (keyword (gensym))]
            [[:rst bios/DCOMPR]
             [:jp :z no-skip-label]
             [:rst bios/DCOMPR]
             [:jp :nc skip-label]
             (label no-skip-label)]))))

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
  [env args]
  (when (> (count args) (count s/args))
    (throw (Exception. "Too many args for new sprite")))
  (mapcat (fn [argvar arg]
            (concat
             [[:ld :hl [argvar]]
              [:push :hl]]
             (load-arg env arg)
             [[:ld [argvar] :hl]]))
          s/args
          args))

(defn- pop-args
  [env args]
  (mapcat (fn [argvar _]
            [[:pop :hl]
             [:ld [argvar] :hl]])
          s/args
          args))

(defn end
  [env]
  [[:ret]])

(defn new-sprite
  [env init-id update-id args]
  (when (> (count args) (count s/args))
    (throw (Exception. "Too many args for new sprite")))
  (concat (push-args env args)
          [[:ld :hl init-id]
           [:ld :de update-id]
           [:call spr/new-sprite]]
          (pop-args env args)))

(defn sprite-delete
  [env]
  [[:call spr/delete-sprite]])

(defn sprite-image
  [env res-id color1-id color2-id]
  [[:ld [:ix spr/+spr-color1+] color1-id]
   [:ld [:ix spr/+spr-color2+] color2-id]
   (set-konami5-page 3 (fn [] (:page (get-label res-id))))
   [:ld :hl res-id]
   [:call spr/write-pattern]])

(defn sprite-animation
  [env res-id]
  [[:ld :hl res-id]
   [:ld [:ix spr/+spr-anim+] :l]
   [:ld [:ix (inc spr/+spr-anim+)] :h]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:ld [:ix (inc spr/+spr-anim-page+)] :a]])

(defn if-keydown
  [env keyname then else]
  (gen-if (fn [l]
            (let [keyname (str/trim (str/upper-case keyname))]
              [(keys/key-down? keyname)
               [:jp :nz l]]))
          then else))

(defn if-keypressed
  [env keyname then else]
  (gen-if (fn [l]
            (let [keyname (str/trim (str/upper-case keyname))
                  keycode (keys/key-codes keyname)]
              [[:ld :e (:row keycode)]
               [:ld :c (:bit keycode)]
               [:call keys/key-pressed?]
               [:jp :nz l]]))
          then else))

(defn if-cmp
  [env id cmp num then else]
  (gen-if (fn [l]
            (compare-code env id num cmp l))
          then else))

(defn if-collide
  [env type then else]
  (gen-if (fn [l]
            (concat
             (load-arg env type)
             [[:ld :a :l]
              [:call spr/collide]
              [:jp :z l]]))
          then else))

(defn if-tile
  [env offset-x offset-y type then else]
  (gen-if (fn [l]
            (concat
             (load-arg env offset-x)
             [[:ld :a :l]
              [:ld :b :a]]
             (load-arg env offset-y)
             [[:ld :a :l]
              [:ld :c :a]
              [:call spr/get-tile]
              [:ld :b :a]]

             (load-arg env type)
             [[:ld :a :l]

              [:cp :b]
              [:jp :nz l]]))
          then else))

(defn load-title
  [env patterns-id colors-id]
  [[:ld :a (fn [] (:page (get-label patterns-id)))]
   [:ld :hl patterns-id]
   [:ld :b (fn [] (:page (get-label colors-id)))]
   [:ld :de colors-id]
   [:call title/load-title]])

(defn return
  [env]
  [[:ret]])

(defn assign
  [env id n]
  (concat
   (load-arg env n)
   (store-arg env id)))

(defn call
  [env func args]
  (concat (push-args env args)
          [[:call func]]
          (pop-args env args)))

(defn animation-end
  [env]
  [[:ld [:ix spr/+spr-anim+] 0]
   [:ld [:ix (inc spr/+spr-anim+)] 0]
   [:ret]])

(defn animation-next-frame
  [env]
  [[:call spr/animation-next-frame]])

(defn animation-play
  [env res-id]
  [[:ld :hl res-id]
   [:ld [:ix spr/+spr-anim+] :l]
   [:ld [:ix (inc spr/+spr-anim+)] :h]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:ld [:ix (inc spr/+spr-anim-page+)] :a]
   [:ret]])

(defn music-play
  [env res-id]
  [[:ld :hl res-id]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:call music/play-music]])

(defn music-stop
  [env]
  [[:call music/init-music]])

(defn sfx-load
  [env res-id]
  [[:ld :hl res-id]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:call music/load-sfx]])

(defn sfx-play
  [env n]
  (concat
   (load-arg env n)
   [[:ld :a :l]
    [:ld :c 0]
    [:call music/play-sfx]]))

(defn tilemap-load
  [env attrs-id lines-id map-id types-id]
  [;; name
   [:call tilemap/clear-name]
   [:di]

   ;; attrs
   (set-konami5-page 3 (fn [] (:page (get-label attrs-id))))
   [:ld :ix attrs-id]
   [:call hscr/load-attrs]

   ;; map
   [:ld :a (fn [] (:page (get-label lines-id)))]
   [:ld :b (fn [] (:page (get-label map-id)))]
   [:ld :c (fn [] (:page (get-label types-id)))]
   [:ld :hl types-id]
   [:ld :ix map-id]
   [:call hscr/load-horizontal-map]
   [:ei]])

(defn scroll-right
  [env]
  [[:call hscr/scroll-right]])

(defn scroll-left
  [env]
  [[:call hscr/scroll-left]])

(defn screen-load
  [env init-id update-id]
  [[:ld :hl init-id]
   [:ld :de update-id]
   [:call scr/load-screen]])

(defn set-tile
  [env x y n]
  (concat
   (load-arg env n)
   [[:ld :a :l]
    [:ld :c :a]]
   (load-arg env y)
   [[:ld :a :l]
    [:ld :b :a]]
   (load-arg env x)
   [[:ld :a :l]
    [:call off/set-tile]]))

(defn write-str
  [env x y str]
  (let [str-label  (keyword (gensym))
        next-label (keyword (gensym))]
    (concat
     [[:ld :de str-label]]
     (load-arg env y)
     [[:ld :a :l]
      [:ld :b :a]]
     (load-arg env x)
     [[:ld :a :l]
      [:call off/write-str]
      [:jr next-label]
      (label str-label (db str) (db 0))
      (label next-label)])))

(defn write-num
  [env x y arg]
  (concat
   (load-arg env arg)
   [[:ex :de :hl]]
   (load-arg env y)
   [[:ld :a :l]
    [:ld :b :a]]
   (load-arg env x)
   [[:ld :a :l]
    [:call off/write-num]]))

(defn write-znum
  [env x y arg]
  (concat
   (load-arg env arg)
   [[:ex :de :hl]]
   (load-arg env y)
   [[:ld :a :l]
    [:ld :b :a]]
   (load-arg env x)
   [[:ld :a :l]
    [:call off/write-znum]]))
