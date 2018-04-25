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

(def ^:dynamic *file* nil)

(declare compile-ops)

(defn- compile-sprite-ops
  [op]
  (match op
         [:new-sprite [:str s] & args]
         [(new-sprite (make-sprite-script-id s :init)
                      (make-sprite-script-id s :update)
                      args)]

         [:sprite-image [:str s]]
         [(sprite-image (make-sprite-id s))]

         [:sprite-pos x y]
         [(sprite-pos x y)]

         [:sprite-move x y]
         [(sprite-move x y)]

         [:sprite-color n]
         [(sprite-color n)]

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
         [(call (make-sprite-script-id (.getName *file*) (keyword s))
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


;; scripts

(let [parser (insta/parser (io/resource "parser.bnf")
                           :string-ci true)]
  (defn script-parser
    [file]
    (let [s (slurp file)]
      (parser s))))

(defn compile-script
  [file]
  (binding [*file* file]
    (let [p (script-parser file)]
      (compile-prog p))))
