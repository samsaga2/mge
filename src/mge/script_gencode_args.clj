(ns mge.script-gencode-args
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [mge.engine-util :as u]
            [mge.script-env :as env]))


(declare load-arg)

(defn- rnd-id?
  [id]
  (= (str/lower-case id) "rnd"))

(defn- load-arg-id
  [env id]
  (if (rnd-id? id)
    [[:call u/random-word]]
    (if-let [v (env/get-env-var env id)]
      (let [i (:addr v)]
        (case (:type v)
          :local [[:ld :l [:ix i]]
                  [:ld :h [:ix (inc i)]]]
          :const [[:ld :hl (:value v)]]
          :global [[:ld :hl [i]]]))
      (throw (Exception. "Uknown variable " id))))) 

(defn- load-arg-num
  [n]
  [[:ld :hl (Integer. n)]])

(defn- load-arg-mul
  [env i j]
  (concat (load-arg env j)
          [[:push :hl]]
          (load-arg env i)
          [[:pop :de]
           [:mult :hl :de]]))

(defn- load-arg-div
  [env i j]
  (concat (load-arg env j)
          [[:push :hl]]
          (load-arg env i)
          [[:pop :de]
           [:div :hl :de]]))

(defn- load-arg-add
  [env i j]
  (concat (load-arg env j)
          [[:push :hl]]
          (load-arg env i)
          [[:pop :de]
           [:add :hl :de]]))

(defn- load-arg-sub
  [env i j]
  (concat (load-arg env j)
          [[:push :hl]]
          (load-arg env i)
          [[:pop :de]
           [:or :a]
           [:sbc :hl :de]])) 

(defn load-arg
  [env arg]
  (match arg
         [:id id]
         (load-arg-id env id) 

         [:num n]
         (load-arg-num n)

         [:arg-mul i j]
         (load-arg-mul env i j)

         [:arg-div i j]
         (load-arg-div env i j)

         [:arg-add i j]
         (load-arg-add env i j)

         [:arg-sub i j]
         (load-arg-sub env i j)))

(defn store-arg
  [env arg]
  (let [type (first arg)
        id   (second arg)]
    (case type
      :id (if-let [v (env/get-env-var env id)]
            (let [i (:addr v)]
              (case (:type v)
                :local  [[:ld [:ix i] :l]
                         [:ld [:ix (inc i)] :h]]
                :global [[:ld [i] :hl]]))
            (throw (Exception. "Uknown variable " id))))))
