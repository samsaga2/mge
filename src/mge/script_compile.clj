(ns mge.script-compile
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [mge.resources-id :refer :all]
            [mge.script-gencode :as gc]
            [mge.engine-script :as s]
            [mge.script-peephole :as ph]
            [mge.script-env :as env]
            [clojure.string :as str]
            [mge.engine-sprites :as spr]
            [clj-z80.asm :refer [make-var]]))


;; script prog

(def ^:dynamic *script-file* nil)

(declare compile-ops)

(defn- assert-res-exists
  [env res-type filename]
  (when-not (env/exists-res env res-type filename)
    (throw (Exception. (str "Resource named `" filename "' of type `" (name res-type) "' cannot be found")))))

(defn- compile-sprite-ops
  [env op]
  (match op
         [:new-sprite [:str s] & args]
         [(gc/new-sprite env
                         (make-sprite-script-id s :init)
                         (make-sprite-script-id s :update)
                         args)]

         [:sprite-delete]
         [(gc/sprite-delete env)]

         :else nil))

(defn- compile-load-ops
  [env op]
  (match op
         [:load "sprite" [:str s]]
         (do (assert-res-exists env :sprites s)
             [(gc/sprite-image env
                               (make-sprite-id s)
                               (make-sprite-color1-id s)
                               (make-sprite-color2-id s))])

         [:load "title" [:str s]]
         (do (assert-res-exists env :titles s)
             [(gc/load-title env
                             (make-title-pattern-id s)
                             (make-title-color-id s))])
         [:load "sfx" [:str s]]
         (do (assert-res-exists env :sfx s)
             [(gc/sfx-load env (make-sfx-id s))])

         [:load "tilemap" [:str s]]
         (do (assert-res-exists env :tilemaps s)
             [(gc/tilemap-load env
                               (make-tilemap-id s :attr)
                               (make-tilemap-id s :lines)
                               (make-tilemap-id s :map)
                               (make-tilemap-id s :types))])

         [:load "screen" [:str s]]
         (do (assert-res-exists env :screen-scripts s)
             [(gc/screen-load env
                              (make-screen-script-id s :init)
                              (make-screen-script-id s :update))])

         :else nil))

(defn- compile-play-ops
  [env op]
  (match op
         [:play-str "animation" [:str s]]
         (do (assert-res-exists env :animation-scripts s)
             [(gc/sprite-animation env (make-animation-script-id s :update))])

         [:play-str "music" [:str s]]
         (do (assert-res-exists env :musics s)
             [(gc/music-play env (make-music-id s))])

         [:play-num "sfx" arg]
         [(gc/sfx-play env arg)]

         [:anim-play [:str s]]
         (do (assert-res-exists env :animation-scripts s)
             [(gc/animation-play env (make-animation-script-id s :update))])

         :else nil))

(defn- compile-misc-ops
  [env op]
  (match op
         [:return]
         [(gc/return env)]

         [:assign id arg]
         [(gc/assign env id arg)]

         [:call [:id s] & args]
         [(gc/call env
                   (make-sprite-script-id (.getName *script-file*)
                                          (keyword s))
                   args)]

         [:next-frame]
         [(gc/animation-next-frame env)]

         [:music-stop]
         [(gc/music-stop env)]

         [:scroll-left]
         [(gc/scroll-left env)]

         [:scroll-right]
         [(gc/scroll-right env)]

         [:set-tile x y n]
         [(gc/set-tile env x y n)]

         [:print-str x y [:str str]]
         [(gc/write-str env x y str)]

         [:print-num x y arg]
         [(gc/write-num env x y arg)]

         [:zprint-num x y arg]
         [(gc/write-znum env x y arg)]

         :else nil))

(defn- compile-cmp
  [env cmp then else]
  (let [then (compile-ops env then)
        else (when-not (empty? else)
               (compile-ops env else))]
    (match cmp
           [:if-keydown [:str key]]
           [(gc/if-keydown env key then else)]

           [:if-keypressed [:str key]]
           [(gc/if-keypressed env key then else)]

           [:if-cmp i j k]
           [(gc/if-cmp env i j k then else)]

           [:if-collide n]
           [(gc/if-collide env n then else)]

           [:if-tile x y type]
           [(gc/if-tile env x y type then else)])))

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
                     (compile-play-ops env op)
                     (compile-misc-ops env op)
                     (compile-if-ops env op)
                     (throw (Exception. (str "Uknown func " op)))))))

(defn- compile-ops
  [env ops]
  (doall (mapcat (partial compile-op env) ops)))

(defn- compile-sub
  [env id ops]
  (let [asm-code (->> (concat (compile-ops (:env @env) ops)
                              (gc/end env))
                      ph/optimize
                      (into []))]
    [(keyword id) asm-code]))

(defn- compile-const
  [env id value]
  (when (get env id)
    (throw (Exception. (str "Variable `" id "' already exists"))))
  (swap! env assoc
         :env (assoc (:env @env)
                     id {:type :const
                         :value value}))
  nil)

(defn- compile-property
  [env id]
  (when (get env id)
    (throw (Exception. (str "Property `" id "' already exists"))))
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

         [:const [:id id] [:num num]]
         (compile-const env id (Integer. num))

         [:property [:id id]]
         (compile-property env id)

         [:global [:id id]]
         (compile-global env id)))

(let [globals (atom {})]
  (defn- compile-script-prog
    [prog resources]
    (let [env (atom {:env          (merge (env/make-env resources) @globals)
                     :globals      globals
                     :local-index  0})]
      (match prog
             [:prog & subs]
             (doall (into {} (map (partial compile-root env) subs)))))))

(defn- add-missing-update
  [script]
  (let [script-subs (set (map first script))]
    (if (not (get script-subs :update))
      (let [empty-update (compile-root (atom {}) [:sub [:id "update"]])]
        (conj script empty-update))
      script)))

;; animation prog

(defn- compile-animation-prog
  [prog resources]
  (let [env (env/make-env resources)]
    (match prog
           [:prog & ops]
           (->> (concat (compile-ops env ops)
                        (gc/animation-end env))
                ph/optimize
                (into [])))))


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
  [file resources]
  (binding [*script-file* file]
    (let [p (script-parser file)]
      (add-missing-update (compile-script-prog p resources)))))

(defn compile-animation
  [file resources]
  (binding [*script-file* file]
    (let [p (animation-parser file)]
      (compile-animation-prog p resources))))


(let [p (insta/parser (io/resource "animation.bnf") :string-ci true)]
  (compile-animation-prog (p "x = (256-16)*8") {}))
