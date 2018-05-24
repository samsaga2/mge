(ns mge.script-compile
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [mge.resources-id :refer :all]
            [mge.script-ir :as ir]
            [clojure.string :as str]))


;; script prog

(def ^:dynamic *script-file* nil)

(declare compile-ops)

(defn- compile-sprite-ops
  [op]
  (match op
         [:new-sprite [:str s] & args]
         [(ir/new-sprite (make-sprite-script-id s :init)
                         (make-sprite-script-id s :update)
                         args)]

         [:sprite-pos x y]
         [(ir/sprite-pos x y)]

         [:sprite-move x y]
         [(ir/sprite-move x y)]

         [:sprite-type n]
         [(ir/sprite-type n)]

         [:sprite-width n]
         [(ir/sprite-width n)]

         [:sprite-height n]
         [(ir/sprite-height n)]

         [:sprite-delete]
         [(ir/sprite-delete)]

         :else nil))

(defn- compile-load-ops
  [op]
  (match op
         [:load type [:str s]]
         (case type
           "image"     [(ir/sprite-image (make-sprite-id s)
                                         (make-sprite-color1-id s)
                                         (make-sprite-color2-id s))]
           "title"     [(ir/load-title (make-title-pattern-id s)
                                       (make-title-color-id s))]
           "animation" [(ir/sprite-animation (make-animation-script-id s :update))]
           "music"     [(ir/music-load (make-music-id s))]
           "sfx"       [(ir/sfx-load (make-sfx-id s))])

         [:anim-load "animation" [:str s]]
         [(ir/animation-load (make-animation-script-id s :update))]

         :else nil))

(defn- compile-misc-ops
  [op]
  (match op
         [:return]
         [(ir/return)]

         [:assign-val id arg]
         [(ir/assign-val id arg)]

         [:assign-add id arg1 arg2]
         [(ir/assign-add id arg1 arg2)]

         [:assign-sub id arg1 arg2]
         [(ir/assign-sub id arg1 arg2)]

         [:call [:id s] & args]
         [(ir/call (make-sprite-script-id (.getName *script-file*)
                                          (keyword s))
                   args)]

         [:next-frame]
         [(ir/animation-next-frame)]

         [:music-stop]
         [(ir/music-stop)]

         [:sfx-play arg]
         [(ir/sfx-play arg)]

         :else nil))

(defn- compile-if-ops
  [op]
  (let [compile-if (fn [condfn then]
                     (if (= (some-> then last first) :else)
                       (let [else (rest (last then))
                             then (drop-last then)]
                         [(condfn
                           (compile-ops then)
                           (compile-ops else))])
                       [(condfn (compile-ops then))]))]
    (match op
           [:if-ops [:if-keydown [:str key]] [:then & then]]
           (compile-if (partial ir/if-keydown key) then)

           [:if-ops [:if-keypressed [:str key]] [:then & then]]
           (compile-if (partial ir/if-keypressed key) then)

           [:if-ops [:if-cmp id cmp n] [:then & then]]
           (compile-if (partial ir/if-cmp id cmp n) then)

           [:if-ops [:if-collide n] [:then & then]]
           (compile-if (partial ir/if-collide n) then)

           :else nil)))

(defn- compile-op
  [op]
  (doall
   (apply concat (or (compile-sprite-ops op)
                     (compile-load-ops op)
                     (compile-misc-ops op)
                     (compile-if-ops op)
                     (throw (Exception. (str "Uknown func " op)))))))

(defn- compile-ops
  [ops]
  (doall (mapcat compile-op ops)))

(defn- compile-sub
  [sub]
  (match sub
         [:sub [:id id] & ops]
         [(keyword id)
          (doall
           (concat (compile-ops ops)
                   (ir/end)))]))

(defn- compile-script-prog
  [prog]
  (match prog
         [:prog & subs]
         (doall (into {} (map compile-sub subs)))))


;; animation prog

(defn- compile-animation-prog
  [prog]
  (match prog
         [:prog & ops]
         (->> (concat (compile-ops ops)
                      (ir/animation-end))
              vec
              doall)))


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
