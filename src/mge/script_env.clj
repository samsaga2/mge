(ns mge.script-env
  (:require [clojure.string :as str]
            [mge.engine-script :as s]
            [mge.engine-sprites :as spr]))

(defn make-func-args-env
  []
  (->> s/args
       (map (fn [g] [(second (str/split (name g) #"---"))
                     {:addr g
                      :type :global}]))
       (into {})))

(defn make-local-env
  []
  (->> {"type"   spr/+spr-type+
        "x"      spr/+spr-x+
        "y"      spr/+spr-y+
        "width"  spr/+spr-w+
        "height" spr/+spr-h+}
       (map (fn [[k v]] [k {:addr v :type :local}]))
       (into {})))

(defn- make-resources-env
  [resources]
  {:res
   {:sprites
    (->> :sprites resources (map #(.getName %)) (into #{}))

    :screen-scripts
    (->> :screen-scripts resources (mapv #(.getName %)) (into #{}))

    :sprite-scripts
    (->> :sprite-scripts resources (mapv #(.getName %)) (into #{}))

    :animation-scripts
    (->> :animation-scripts resources (mapv #(.getName %)) (into #{}))

    :titles
    (->> :titles resources (mapv #(.getName %)) (into #{}))

    :musics
    (->> :musics resources (mapv #(.getName %)) (into #{}))

    :sfx
    (->> :sfx resources (mapv #(.getName %)) (into #{}))

    :tilemaps
    (->> :tilemaps resources (mapv #(.getName %)) (into #{}))}})

(defn make-env
  [resources]
  (let [args-vars  (make-func-args-env)
        local-vars (make-local-env)
        res-vars   (make-resources-env resources)]
    (merge args-vars local-vars res-vars)))

(defn get-env-var
  [env id]
  (or (get env id)
      (throw (Exception. (str "Variable " id " not found")))))

(defn exists-res
  [env res-type filename]
  (contains? (get-in env [:res res-type])
             filename))
  
