(ns mge.resources-id)

(defn make-sprite-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spr-" base-name))))

(defn make-sprite-color1-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spr-" base-name "-color1"))))

(defn make-sprite-color2-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-spr-" base-name "-color2"))))

(defn make-screen-script-id
  ([filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-screenscr-" base-name))))
  ([filename func]
   (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
     (keyword (str "res-screenscr-" base-name "-" (name func))))))

(defn make-sprite-script-id
  ([filename]
   (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
     (keyword (str "res-spritescr-" base-name))))
  ([filename func]
   (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
     (keyword (str "res-spritescr-" base-name "-" (name func))))))

(defn make-title-pattern-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-titlepat-" base-name))))

(defn make-title-color-id
  [filename]
  (let [base-name (subs filename 0 (.lastIndexOf filename "."))]
    (keyword (str "res-titlecol-" base-name))))
