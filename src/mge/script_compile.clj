(ns mge.script-compile
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [mge.resources-id :refer :all]
            [mge.script-ir :refer :all]))


;; ops

(def ^:dynamic *script-file* nil)

(declare compile-ops)

(defn- compile-sprite-ops
  [op]
  (match op
         [:new-sprite [:str s] & args]
         [(new-sprite (make-sprite-script-id s :init)
                      (make-sprite-script-id s :update)
                      args)]

         [:sprite-image [:str s]]
         [(sprite-image (make-sprite-id s)
                        (make-sprite-color1-id s)
                        (make-sprite-color2-id s))]

         [:sprite-pos x y]
         [(sprite-pos x y)]

         [:sprite-move x y]
         [(sprite-move x y)]

         [:sprite-type n]
         [(sprite-type n)]

         [:sprite-width n]
         [(sprite-width n)]

         [:sprite-height n]
         [(sprite-height n)]

         [:sprite-delete]
         [(sprite-delete)]

         :else nil))

(defn- compile-misc-ops
  [op]
  (match op
         [:title [:str s]]
         [(load-title (make-title-pattern-id s)
                      (make-title-color-id s))]

         [:return]
         [(return)]

         [:assign-val id arg]
         [(assign-val id arg)]

         [:assign-add id arg1 arg2]
         [(assign-add id arg1 arg2)]

         [:assign-sub id arg1 arg2]
         [(assign-sub id arg1 arg2)]

         [:call [:id s] & args]
         [(call (make-sprite-script-id (.getName *script-file*)
                                       (keyword s))
                args)]

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
           (compile-if (partial if-keydown key) then)

           [:if-ops [:if-keypressed [:str key]] [:then & then]]
           (compile-if (partial if-keypressed key) then)

           [:if-ops [:if-cmp id cmp n] [:then & then]]
           (compile-if (partial if-cmp id cmp n) then)

           [:if-ops [:if-collide n] [:then & then]]
           (compile-if (partial if-collide n) then)

           :else nil)))

(defn- compile-op
  [op]
  (doall
   (apply concat (or (compile-sprite-ops op)
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
                   (end)))]))

(defn- compile-prog
  [prog]
  (match prog
         [:prog & subs]
         (doall (into {} (map compile-sub subs)))))


;; core

(let [parser (insta/parser (io/resource "parser.bnf")
                           :string-ci true)]
  (defn script-parser
    [file]
    (let [s (slurp file)]
      (parser s))))

(defn compile-script
  [file]
  (binding [*script-file* file]
    (let [p (script-parser file)]
      (compile-prog p))))
