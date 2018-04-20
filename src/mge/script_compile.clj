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

         :else nil))

(defn- compile-new-ops
  [op]
  (match op
         [:new-sprite [:str s]]
         [(new-sprite (make-sprite-script-id s :init)
                      (make-sprite-script-id s :update))]

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
           [(if-cmp id cmp (Integer. n) (compile-ops then))])))

(defn- compile-op
  [op]
  (apply concat (or (compile-sprite-ops op)
                    (compile-new-ops op)
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
