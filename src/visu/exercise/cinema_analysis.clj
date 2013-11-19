(ns visu.exercise.cinema-analysis
  (:require [clojure.string :refer [split]])
  (:use quil.core))

(def data-path "data/kill_bill.csv")
(def sketch-width 1100)
(def sketch-height 700)


(def diagram-state
  (atom
   {:x-start 50
    :x-end nil
    :y-start nil
    :y-end 50
    :gross-scale (/ (- sketch-height 100) 35000000)
    :rank-scale (/ (- sketch-height 100) 70)
    :step nil
    :data nil}))


(defn read-data [path]
  (let [lines (split (slurp path) #"\n")]
    (map #(split % #",") lines)))


(defn prepare-data [data]
  (let [column-names (first data)
        raw-data (map (fn [x] (into {} (map vector column-names x))) (rest data))
        integer-ranks (map #(assoc % "Rank" (Integer/parseInt (% "Rank"))) raw-data)
        integer-gross (map #(assoc % "Weekly Gross" (Integer/parseInt (% "Weekly Gross"))) integer-ranks)]
    integer-gross))


(defn draw-scales [state]
  (let [x-start (state :x-start)
        x-end (state :x-end)
        y-start (state :y-start)
        y-end (state :y-end)
        gross-scale (state :gross-scale)
        rank-scale (state :rank-scale)]
    (line x-start y-start x-start y-end)
    (line x-end y-start x-end y-end)
    (line x-start y-start x-end y-start)
    (fill 0)
    (text "Gross [Mio $](red)" x-start (- y-end 5))
    (text-align :right)
    (text "Rank (green)" x-end (- y-end 5))
    (dorun
     (for [i (range 8)]
       (do
         (line (- x-start 3) (- y-start (* i 5000000 gross-scale)) (+ x-start 3) (- y-start (* i 5000000 gross-scale)))
         (line (- x-end 3) (- y-start (* i 10 rank-scale)) (+ x-end 3) (- y-start (* i 10 rank-scale)))
         (fill 0)
         (text-align :right)
         (text (str (* i 5)) (- x-start 10) (- y-start (* i 5000000 gross-scale)))
         (text-align :left)
         (text (str (* i 10)) (+ x-end 10) (- y-start (* i 10 rank-scale)))
         ))))
  state)


(defn draw-gross-data [state]
  (let [x-start (state :x-start)
        y-start (state :y-start)
        data (state :data)
        step (state :step)
        weekly-gross (apply vector (map #(% "Weekly Gross") data))
        weeks (apply vector (map #(% "Week") data))
        scale (state :gross-scale)]
    (dorun
     (for [i (range (count data))]
       (do
         (stroke 0)
         (fill 255 0 0)
         (ellipse
          (+ x-start (* i step) (/ step 2)) (- y-start (* (weekly-gross i) scale))
          5 5)
         (fill 0)
         (text-align :center)
         (text (weeks i) (+ x-start (* i step) (/ step 2)) (+ y-start 30))))))
  state)


(defn draw-rank-data [state]
  (let [x-start (state :x-start)
        x-end (state :x-end)
        y-start (state :y-start)
        data (state :data)
        step (state :step)
        weekly-rank (apply vector (map #(% "Rank") data))
        scale (state :rank-scale)]
    (dorun
     (for [i (range (count data))]
       (do
         (fill 0 255 0)
         (ellipse
          (+ x-start (* i step) (/ step 2)) (- y-start (* (weekly-rank i) scale))
          5 5))))
    (stroke-weight 2)
    (stroke 0 255 0)
    (line x-start (- y-start (* 10 scale)) x-end (- y-start (* 10 scale))))
  state)


(defn setup []
  (let [data (prepare-data (read-data data-path))]
    (swap! diagram-state assoc
           :data data
           :step (/ (- sketch-width 100) (count data))
           :x-end (- sketch-width 50)
           :y-start (- sketch-height 50))
    (background 255 40)
    (smooth)
    (-> (deref diagram-state)
        draw-scales
        draw-rank-data
        draw-gross-data)
    (text-align :center)
    (text-size 17)
    (text "Weekly Gross and Rank of Neo Noir Movies" (/ sketch-width 2) 50)))


(defsketch cinema-analysis-screen-1
  :title "Analyse von Kinodaten"
  :setup setup
  :size [sketch-width sketch-height])
