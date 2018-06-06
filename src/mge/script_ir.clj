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
            [mge.math :as m]
            [mge.music :as music]
            [mge.tilemap :as tilemap]
            [mge.screens :as scr]
            [mge.offscreen :as off]))


;; env

(defn default-env
  []
  (let [global-vars (->> s/args
                         (map (fn [g] [(second (str/split (name g) #"---"))
                                       {:addr g
                                        :type :global}]))
                         (into {}))
        local-vars  {"type"   spr/+spr-type+
                     "x"      spr/+spr-x+
                     "y"      spr/+spr-y+
                     "width"  spr/+spr-w+
                     "height" spr/+spr-h+}
        local-vars  (->> local-vars
                         (map (fn [[k v]] [k {:addr v
                                              :type :local}]))
                         (into {}))]
    (merge global-vars local-vars)))

(defn- get-env-var
  [env id]
  (or (get env id)
      (throw (Exception. (str "Variable " id " not found")))))


;; args

(defn- load-arg
  [env arg]
  (let [type (first arg)
        id   (second arg)]
    (case type
      :id  (if (= (str/lower-case id) "rnd")
             [[:call u/random-word]]
             (if-let [v (get-env-var env id)]
               (let [i (:addr v)]
                 (case (:type v)
                   :local [[:ld :l [:ix i]]
                           [:ld :h [:ix (inc i)]]]
                   :global [[:ld :hl [i]]]))
               (throw (Exception. "Uknown variable " id))))
      :num [[:ld :hl (Integer. id)]])))

(defn- store-arg
  [env arg]
  (let [type (first arg)
        id   (second arg)]
    (case type
      :id (if-let [v (get-env-var env id)]
            (let [i (:addr v)]
              (case (:type v)
                :local  [[:ld [:ix i] :l]
                         [:ld [:ix (inc i)] :h]]
                :global [[:ld [i] :hl]]))
            (throw (Exception. "Uknown variable " id))))))


;; util

(defn- compare-code
  [env i j cmp skip-label]
  (concat (if (= cmp "<=")
            [(load-arg env j)
             [:push :hl]
             (load-arg env i)
             [:ex :de :hl]
             [:pop :hl]]
            [(load-arg env i)
             [:push :hl]
             (load-arg env j)
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
  [env args]
  (when (> (count args) (count s/args))
    (throw (Exception. "Too many args for new sprite")))
  (mapcat (fn [argvar arg]
            [[:ld :hl [argvar]]
             [:push :hl]
             (load-arg env arg)
             [:ld [argvar] :hl]])
          s/args
          args))

(defn- pop-args
  [env args]
  (mapcat (fn [argvar _]
            [[:pop :hl]
             [:ld [argvar] :hl]])
          s/args
          args))


;; core

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

(defn sprite-pos
  [env x y]
  [(load-arg env x)
   [:ld [:ix spr/+spr-x+] :l]
   [:ld [:ix (inc spr/+spr-x+)] :h]
   (load-arg env y)
   [:ld [:ix spr/+spr-y+] :l]
   [:ld [:ix (inc spr/+spr-y+)] :h]])

(defn sprite-move
  [env x y]
  [[:ld :e [:ix spr/+spr-x+]]
   [:ld :d [:ix (inc spr/+spr-x+)]]
   (load-arg env x)
   [:add :hl :de]
   [:ld [:ix spr/+spr-x+] :l]
   [:ld [:ix (inc spr/+spr-x+)] :h]

   [:ld :e [:ix spr/+spr-y+]]
   [:ld :d [:ix (inc spr/+spr-y+)]]
   (load-arg env y)
   [:add :hl :de]
   [:ld [:ix spr/+spr-y+] :l]
   [:ld [:ix (inc spr/+spr-y+)] :h]])

(defn sprite-type
  [env n]
  [(load-arg env n)
   [:ld [:ix spr/+spr-type+] :l]])

(defn sprite-width
  [env n]
  [(load-arg env n)
   [:ld [:ix spr/+spr-w+] :l]])

(defn sprite-height
  [env n]
  [(load-arg env n)
   [:ld [:ix spr/+spr-h+] :l]])

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
            [(load-arg env type)
             [:ld :a :l]
             [:call spr/collide]
             [:jp :z l]])
          then else))

(defn if-tile
  [env offset-x offset-y type then else]
  (gen-if (fn [l]
            [(load-arg env offset-x)
             [:ld :a :l]
             [:ld :b :a]
             (load-arg env offset-y)
             [:ld :a :l]
             [:ld :c :a]
             [:call spr/get-tile]
             [:ld :b :a]

             (load-arg env type)
             [:ld :a :l]

             [:cp :b]
             [:jp :nz l]])
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

(defn assign-val
  [env id n]
  [(load-arg env n)
   (store-arg env id)])

(defn assign-add
  [env id arg1 arg2]
  [(load-arg env arg2)
   [:ex :de :hl]
   (load-arg env arg1)
   [:add :hl :de]
   (store-arg env id)])

(defn assign-sub
  [env id arg1 arg2]
  [(load-arg env arg2)
   [:ex :de :hl]
   (load-arg env arg1)
   [:or :a]
   [:sbc :hl :de]
   (store-arg env id)])

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

(defn animation-load
  [env res-id]
  [[:ld :hl res-id]
   [:ld [:ix spr/+spr-anim+] :l]
   [:ld [:ix (inc spr/+spr-anim+)] :h]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:ld [:ix (inc spr/+spr-anim-page+)] :a]
   [:ret]])

(defn music-load
  [env res-id]
  [[:ld :hl res-id]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:call music/play-music]])

(defn music-stop
  [env res-id]
  [[:call music/stop-music]])

(defn sfx-load
  [env res-id]
  [[:ld :hl res-id]
   [:ld :a (fn [] (:page (get-label res-id)))]
   [:call music/load-sfx]])

(defn sfx-play
  [env n]
  [(load-arg env n)
   [:ld :a :l]
   [:ld :c 0]
   [:call music/play-sfx]])

(defn tilemap-load
  [env patterns-id colors-id attrs-id lines-id map-id types-id]
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
  [env]
  [[:call tilemap/scroll-right]])

(defn scroll-left
  [env]
  [[:call tilemap/scroll-left]])

(defn screen-load
  [env init-id update-id]
  [[:ld :hl init-id]
   [:ld :de update-id]
   [:call scr/load-screen]])

(defn set-tile
  [env x y n]
  [(load-arg env n)
   [:ld :a :l]
   [:ld :c :a]
   (load-arg env y)
   [:ld :a :l]
   [:ld :b :a]
   (load-arg env x)
   [:ld :a :l]
   [:call off/set-tile]])

(defn write-str
  [env x y str]
  (let [str-label  (keyword (gensym))
        next-label (keyword (gensym))]
    [[:ld :de str-label]
     (load-arg env y)
     [:ld :a :l]
     [:ld :b :a]
     (load-arg env x)
     [:ld :a :l]
     [:call off/write-print]
     [:jr next-label]
     (label str-label (db str) (db 0))
     (label next-label)]))
