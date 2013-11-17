(ns visu.exercise.cinema-analysis
  (:require [clojure.string :refer [split]])
  (:use quil.core))

(def data-path "data/kill_bill.csv")

(defn read-data [path]
  (let [lines (split (slurp path) #"\n")]
    (map #(split % #",") lines)))

(defn prepare-data [data]
  (let [column-names (first data)
        raw-data (map (fn [x] (into {} (map vector column-names x))) (rest data))
        integer-ranks (map #(assoc % "Rank" (Integer/parseInt (% "Rank"))) raw-data)
        integer-gross (map #(assoc % "Weekly Gross" (Integer/parseInt (% "Weekly Gross"))) integer-ranks)]
    integer-gross))


(defn draw-gross-data [data]
  (let [x-start 10
        y-start 750
        step (/ 1000 (count data))
        weekly-gross (apply vector (map #(% "Weekly Gross") data))
        weeks (apply vector (map #(% "Week") data))
        scale (/ 700 35000000)]
    (dorun
     (for [i (range (count data))]
       (do
         (fill 255 0 0)
         (ellipse
          (+ x-start (* i step) (/ step 2)) (- y-start (* (weekly-gross i) scale))
          5 5)
         (fill 0)
         (text-align :center)
         (text (weeks i) (+ x-start (* i step) (/ step 2)) (+ y-start 30)))))))


(defn draw-rank-data [data]
  (let [x-start 10
        y-start 750
        step (/ 1000 (count data))
        weekly-rank (apply vector (map #(% "Rank") data))
        scale (/ 700 60)]
    (dorun
     (for [i (range (count data))]
       (do
         (fill 0 255 0)
         (ellipse
          (+ x-start (* i step) (/ step 2)) (- y-start (* (weekly-rank i) scale))
          5 5))))
    (fill 0 255 0)
    (line x-start (- y-start (* 10 scale)) (+ x-start 1000) (- y-start (* 10 scale)))))


(defn setup []
  (let [data (prepare-data (read-data data-path))]
    (size 1000 800)
    (background 255 40)
    (smooth)
    (draw-gross-data data)
    (draw-rank-data data)))

(defsketch cinema-analysis-screen-1
  :title "Analyse von Kinodaten"
  :setup setup
  :size [1100 800])
