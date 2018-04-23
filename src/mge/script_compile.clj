(ns mge.script-compile
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [mge.script-ir :refer :all]
            [clojure.java.io :as io]))


;; ids

(defn make-sprite-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spr-" base-name))))

(defn make-screen-script-id
  [filename func]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-screenscr-" base-name "-" (name func)))))

(defn make-sprite-script-id
  [filename func]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spritescr-" base-name "-" (name func)))))

(defn make-title-pattern-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-titlepat-" base-name))))

(defn make-title-color-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-titlecol-" base-name))))


;; ops

(declare compile-ops)

(defn- compile-sprite-ops
  [op]
  (match op
         [:sprite-image [:str s]]
         [(sprite-image (make-sprite-id s))]

         [:sprite-pos [:num x] [:num y]]
         [(sprite-pos (Integer. x) (Integer. y))]

         [:sprite-move [:num x] [:num y]]
         [(sprite-move (Integer. x) (Integer. y))]

         [:sprite-color [:num n]]
         [(sprite-color (Integer. n))]

         [:sprite-type [:num n]]
         [(sprite-type (Integer. n))]

         [:sprite-width [:num n]]
         [(sprite-width (Integer. n))]

         [:sprite-height [:num n]]
         [(sprite-height (Integer. n))]

         :else nil))

(defn- compile-screen-ops
  [op]
  (match op
         [:new-sprite [:str s]]
         [(new-sprite (make-sprite-script-id s :init)
                      (make-sprite-script-id s :update))]

         [:title [:str s]]
         [(load-title (make-title-pattern-id s)
                      (make-title-color-id s))]

         :else nil))

(defn- compile-if-ops
  [op]
  (match op
         [:if-ops [:if-keydown [:str key]] [:then & then]]
         (if (= (some-> then last first) :else)
           (let [else (rest (last then))
                 then (drop-last then)]
             [(if-keydown key
                          (compile-ops then)
                          (compile-ops else))])
           [(if-keydown key (compile-ops then))])

         [:if-ops [:if-cmp id cmp [:num n]] [:then & then]]
         (if (= (some-> then last first) :else)
           (let [else (rest (last then))
                 then (drop-last then)]
             [(if-cmp id cmp (Integer. n)
                      (compile-ops then)
                      (compile-ops else))])
           [(if-cmp id cmp (Integer. n) (compile-ops then))])

         [:if-ops [:if-collide [:num n]] [:then & then]]
         (if (= (some-> then last first) :else)
           (let [else (rest (last then))
                 then (drop-last then)]
             [(if-collide (Integer. n)
                          (compile-ops then)
                          (compile-ops else))])
           [(if-collide (Integer. n) (compile-ops then))])

         :else nil))

(defn- compile-op
  [op]
  (apply concat (or (compile-sprite-ops op)
                    (compile-screen-ops op)
                    (compile-if-ops op)
                    (throw (Exception. (str "Uknown func " op))))))

(defn- compile-ops
  [ops]
  (mapcat compile-op ops))

(defn- compile-sub
  [sub]
  (match sub
         [:sub id & ops]
         [(keyword id) (concat (compile-ops ops)
                               [(end)])]))


;; screen scripts

(def screen-parser
  (insta/parser (io/resource "screen_parser.bnf")
                :string-ci true))

(defn compile-screen-script
  [script]
  (let [p (screen-parser script)]
    (match p
           [:prog & subs]
           (into {} (map compile-sub subs)))))


;; sprite scripts

(def sprite-parser
  (insta/parser (io/resource "sprite_parser.bnf")
                :string-ci true))

(defn compile-sprite-script
  [script]
  (let [p (sprite-parser script)]
    (match p
           [:prog & subs]
           (into {} (map compile-sub subs)))))
