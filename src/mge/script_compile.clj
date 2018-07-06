(ns mge.script-compile
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [mge.resources-id :refer :all]
            [mge.script-ir :as ir]
            [mge.script :as s]
            [clojure.string :as str]
            [mge.sprites :as spr]
            [clj-z80.asm :refer [make-var]]))


;; script prog

(def ^:dynamic *script-file* nil)

(declare compile-ops)

(defn- compile-sprite-ops
  [env op]
  (match op
         [:new-sprite [:str s] & args]
         [(ir/new-sprite env
                         (make-sprite-script-id s :init)
                         (make-sprite-script-id s :update)
                         args)]

         [:sprite-delete]
         [(ir/sprite-delete env)]

         :else nil))

(defn- compile-load-ops
  [env op]
  (match op
         [:load type [:str s]]
         (case type
           "image"     [(ir/sprite-image env
                                         (make-sprite-id s)
                                         (make-sprite-color1-id s)
                                         (make-sprite-color2-id s))]
           "title"     [(ir/load-title env
                                       (make-title-pattern-id s)
                                       (make-title-color-id s))]
           "animation" [(ir/sprite-animation env (make-animation-script-id s :update))]
           "music"     [(ir/music-load env (make-music-id s))]
           "sfx"       [(ir/sfx-load env (make-sfx-id s))]
           "tilemap"   [(ir/tilemap-load env
                                         (make-tilemap-id s :pattern)
                                         (make-tilemap-id s :colors)
                                         (make-tilemap-id s :attr)
                                         (make-tilemap-id s :lines)
                                         (make-tilemap-id s :map)
                                         (make-tilemap-id s :types))]
           "screen" [(ir/screen-load env
                                     (make-screen-script-id s :init)
                                     (make-screen-script-id s :update))])

         [:anim-load "animation" [:str s]]
         [(ir/animation-load env (make-animation-script-id s :update))]

         :else nil))

(defn- compile-misc-ops
  [env op]
  (match op
         [:return]
         [(ir/return env)]

         [:assign-val id arg]
         [(ir/assign-val env id arg)]

         [:assign-add id arg1 arg2]
         [(ir/assign-add env id arg1 arg2)]

         [:assign-sub id arg1 arg2]
         [(ir/assign-sub env id arg1 arg2)]

         [:call [:id s] & args]
         [(ir/call env
                   (make-sprite-script-id (.getName *script-file*)
                                          (keyword s))
                   args)]

         [:next-frame]
         [(ir/animation-next-frame env)]

         [:music-stop]
         [(ir/music-stop env)]

         [:sfx-play arg]
         [(ir/sfx-play env arg)]

         [:scroll-left]
         [(ir/scroll-left env)]

         [:scroll-right]
         [(ir/scroll-right env)]

         [:set-tile x y n]
         [(ir/set-tile env x y n)]

         [:print-str x y [:str str]]
         [(ir/write-str env x y str)]

         [:print-num x y arg]
         [(ir/write-num env x y arg)]

         [:zprint-num x y arg]
         [(ir/write-znum env x y arg)]

         :else nil))

(defn- compile-cmp
  [env cmp then else]
  (let [then (compile-ops env then)
        else (when-not (empty? else)
               (compile-ops env else))]
    (match cmp
           [:if-keydown [:str key]]
           [(ir/if-keydown env key then else)]

           [:if-keypressed [:str key]]
           [(ir/if-keypressed env key then else)]

           [:if-cmp i j k]
           [(ir/if-cmp env i j k then else)]

           [:if-collide n]
           [(ir/if-collide env n then else)]

           [:if-tile x y type]
           [(ir/if-tile env x y type then else)])))

(defn- compile-if-ops
  [env op]
  (match op
         [:if-ops cmp [:then & then]]
         (compile-cmp env cmp then nil)

         [:if-ops cmp [:then & then] [:else & else]]
         (compile-cmp env cmp then else)

         :else nil))

(defn- compile-op
  [env op]
  (doall
   (apply concat (or (compile-sprite-ops env op)
                     (compile-load-ops env op)
                     (compile-misc-ops env op)
                     (compile-if-ops env op)
                     (throw (Exception. (str "Uknown func " op)))))))

(defn- compile-ops
  [env ops]
  (doall (mapcat (partial compile-op env) ops)))

(defn- compile-sub
  [env id ops]
  [(keyword id)
   (doall
    (concat (compile-ops (:env @env) ops)
            (ir/end env)))])

(defn- compile-property
  [env id]
  (let [local-index (inc (:local-index @env))]
    (when (== local-index 16)
      (throw (Exception. "Too many properties")))
    (swap! env assoc
           :local-index local-index
           :env (assoc (:env @env)
                       id {:type :local
                           :addr (+ spr/+spr-local0+
                                    (* (dec local-index) 2))}))
    nil))

(defn- compile-global
  [env id]
  (let [globals (:globals @env)]
    (when-not (get @globals id)
      (let [var-id (keyword (gensym))
            global {:type :global
                    :addr var-id}]
        (make-var var-id 2)
        (swap! globals assoc id global)
        (swap! env assoc :env (assoc (:env @env) id global)))
      nil)))

(defn- compile-root
  [env sub]
  (match sub
         [:sub [:id id] & ops]
         (compile-sub env id ops)

         [:property [:id id]]
         (compile-property env id)

         [:global [:id id]]
         (compile-global env id)))

(let [globals (atom {})]
  (defn- compile-script-prog
    [prog]
    (let [env (atom {:env          (merge (ir/default-env) @globals)
                     :globals      globals
                     :local-index  0})]
      (match prog
             [:prog & subs]
             (doall (into {} (map (partial compile-root env) subs)))))))


;; animation prog

(defn- compile-animation-prog
  [prog]
  (let [env (ir/default-env)]
    (match prog
           [:prog & ops]
           (->> (concat (compile-ops env ops)
                        (ir/animation-end env))
                vec
                doall))))


;; core
(defn- make-parser
  [parser file]
  (let [s (parser (slurp file))]
    (if (insta/failure? s)
      (do (println "Error parsing script" (.getPath file) ": " (insta/get-failure s))
          [:prog])
      s)))

(let [script    (insta/parser (io/resource "script.bnf") :string-ci true)
      animation (insta/parser (io/resource "animation.bnf") :string-ci true)]
  (defn script-parser
    [file]
    (make-parser script file))
  (defn animation-parser
    [file]
    (make-parser animation file)))

(defn compile-script
  [file]
  (binding [*script-file* file]
    (let [p (script-parser file)]
      (compile-script-prog p))))

(defn compile-animation
  [file]
  (binding [*script-file* file]
    (let [p (animation-parser file)]
      (compile-animation-prog p))))
