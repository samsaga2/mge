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

(defn- compile-op
  [op]
  (apply concat (or (compile-sprite-ops op)
                    (compile-new-ops op)
                    (throw (Exception. (str "Uknown func " op))))))

(defn- compile-ops
  [ops]
  (apply concat
         (mapcat compile-op ops)
         [(end)]))

(defn- compile-sub
  [sub]
  (match sub
         [:sub id & ops]
         [(keyword id) (compile-ops ops)]))


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
