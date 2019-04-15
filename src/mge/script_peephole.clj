(ns mge.script-peephole
  (:require [clojure.core.match :refer [match]]))

(defn- optimize-pass
  [asm]
  (match (into [] asm)

         [[:push :hl]
          [:ld :de n]
          [:pop :hl]
          & tl]
         (concat [[:ld :de n]]
                 (optimize-pass tl))

         [[:ld :hl n1]
          [:push :hl]
          [:ld :hl n2]
          [:pop :de]
          & tl]
         (concat [[:ld :de n1]
                  [:ld :hl n2]]
                 (optimize-pass tl))

         [[:ld :hl n1]
          [:push :hl]
          [:ld :hl n2]
          [:ex :de :hl]
          [:pop :hl]
          & tl]
         (concat [[:ld :hl n1]
                  [:ld :de n2]]
                 (optimize-pass tl))

         [[:ld [n1] :hl]
          [:ld :hl [n2]]
          & tl]
         (if (= n1 n2)
           (concat [[:ld [n1] :hl]]
                   (optimize-pass tl))
           (concat [[:ld [n1] :hl]
                    [:ld :hl [n2]]]
                   (optimize-pass tl)))

         [[:ld :hl n]
          [:ex :de :hl]
          & tl]
         (concat [[:ld :de n]]
                 (optimize-pass tl))

         [[:push :hl]
          [:ld :de n]
          [:pop :hl]
          & tl]
         (concat [[:ld :de n]]
                 (optimize-pass tl))

         [[:call f]
          [:ret]
          & tl]
         (concat [[:jp f]]
                 (optimize-pass tl))

         [[:push :hl]
          [:ld :l n1]
          [:ld :h n2]
          [:pop :de]
          & tl]
         (concat [[:ex :de :hl]
                  [:ld :l n1]
                  [:ld :h n2]]
                 (optimize-pass tl))

         [[:ld :de 1]
          [:ld :hl n]
          [:or :a]
          [:sbc :hl :de]
          & tl]
         (concat [[:ld :hl n]
                  [:dec :hl]]
                 (optimize-pass tl))

         [[:ld :de 1]
          [:ld :hl n]
          [:add :hl :de]
          & tl]
         (concat [[:ld :hl n]
                  [:inc :hl]]
                 (optimize-pass tl))

         [[:ld :de n1]
          [:ld :hl n2]
          [:or :a]
          [:sbc :hl :de]
          & tl]
         (concat [[:ld :hl (- n2 n1)]]
                 (optimize-pass tl))

         [[:ld :de n1]
          [:ld :hl n2]
          [:add :hl :de]
          & tl]
         (concat [[:ld :hl (+ n2 n1)]]
                 (optimize-pass tl))

         [[:ld :de n1]
          [:ld :hl n2]
          [:mult :hl :de]
          & tl]
         (concat [[:ld :hl (* n2 n1)]]
                 (optimize-pass tl))

         [[:ld :de n1]
          [:ld :hl n2]
          [:div :hl :de]
          & tl]
         (concat [[:ld :hl (int (/ n2 n1))]]
                 (optimize-pass tl))

         ;; finish peephole
         []
         []

         ;; optimize next line
         [hd & tl]
         (concat [hd]
                 (optimize-pass tl)))) 

(defn optimize
  [asm]
  (loop [asm   asm
         count 0]
    (let [new-asm (optimize-pass asm)]
      (if (or (= new-asm asm)
              (= count 100))
        new-asm
        (recur new-asm (inc count))))))
