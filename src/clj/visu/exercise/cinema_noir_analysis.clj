(ns visu.exercise.cinema-noir-analysis
  (:use quil.core)
  (:require [clojure.string :refer [split]]))

(def data-path "data/neo_noir.csv")
(def films [0 1 2 3 5 7 10 12 13 14 15 16 17 19 26 27 28 30 33 34 35 40 41 44 45 53 56 60 63 66])
(def sketch-width 1100)
(def sketch-height 700)


(def diagram-state
  (atom
   {:x-start 50
    :y-start (- sketch-height 50)
    :x-end (- sketch-width 50)
    :y-end 50
    :start-year 1980
    :year-span 34
    :y-scale (/ (- sketch-height 100) 75000000)
    :step nil
    :data nil
    :selected nil}))


(defn generate-random-set [random-set-size random-set-range]
  (loop [random-set #{}]
    (if (not (= random-set-size (count random-set)))
      (recur (into #{} (conj random-set (rand-int random-set-range))))
      random-set)))


(defn read-data [path]
  (let [lines (split (slurp path) #"\n")]
    (map #(split % #",") lines)))


(defn parse-date [date]
  (let [date-string (split date #"/")
        year (Integer/parseInt (last date-string))]
    (if (< year 79)
      (+ 2000 year)
      (+ 1900 year))))


(defn prepare-data [data]
  (let [column-names (first data)
        raw-data (apply vector (map (fn [x] (into {} (map vector column-names x))) (rest data)))
        selected-data (map #(raw-data %) films)
        integer-gross (map #(assoc % "Overall_Gross" (Integer/parseInt (% "Overall_Gross"))) selected-data)
        integer-dates (map #(assoc % "Date" (parse-date (% "Date"))) integer-gross)]
    (apply vector integer-dates)))


(defn find-data [x y state]
  (let [step (state :step)
        x-start (state :x-start)
        y-start (state :y-start)
        year (+ (/ (- x x-start) step) 1980)
        gross (/ (- y-start y) (state :y-scale))
        data (state :data)]
    (filter
       #(and
         (<= (* 0.9995 year) (% "Date") (* 1.0005 year))
         (<= (* 0.9 gross) (% "Overall_Gross") (* 1.1 gross)))
       data)))


(defn draw-gross-data [state]
  (let [data (state :data)
        x-start (state :x-start)
        y-start (state :y-start)
        start-year (state :start-year)
        year-span (state :year-span)
        step (state :step)
        scale (state :y-scale)]
    (dorun
     (for [i (range (count data))]
       (let [x (+ x-start (* (- ((data i) "Date") start-year) step) (/ step 2))
             y (- y-start (* scale ((data i) "Overall_Gross")))]
         (do
           (fill 255 0 0)
           (ellipse x y 5 5)
           (fill 0)
           (text ((data i) "Title") x (- y 5))
           )))))
  state)


(defn draw-scales [state]
  (let [x-start (state :x-start)
        y-start (state :y-start)
        x-end (state :x-end)
        y-end (state :y-end)
        start-year (state :start-year)
        year-span (state :year-span)
        step (state :step)
        scale (state :y-scale)]
    (do
      (line x-start y-start x-end y-start)

      (dorun
       (for [i (range 7)]
         (do
           (fill 0)
           (text-align :center)
           (text (str (+ 1980 (* i 5))) (+ x-start (* (* i 5) step) (/ step 2)) (+ y-start 30)))))

      (dorun
       (for [i (range year-span)]
         (let [x (+ x-start (* i step) (/ step 2))]
           (line x (+ y-start 5) x (- y-start 5)))))

      (line (+ x-start (/ step 2)) y-start
            (+ x-start (/ step 2)) y-end)

      (dorun
       (for [i (range 16)]
         (let [x (+ x-start (/ step 2))
               y (- y-start (* i 5000000 scale))]
           (do
             (line (- x 5) y (+ x 5) y)
             (text-align :center)
             (text (str (* i 5)) (- x 15) y)))))))
  state)


(defn setup []
  (let [data (prepare-data (read-data data-path))]
    (swap! diagram-state
           assoc
           :data data
           :step  (/ (- sketch-width 100) 34))
    (background 255 40)
    (smooth)
    (set-state! :mouse-position (atom [0 0]))
    (-> (deref diagram-state)
        draw-scales
        draw-gross-data)))


(defn draw []
  (let [[x y] @(state :mouse-position)
        selected ((deref diagram-state) :selected)]
    (fill 255)
    (no-stroke)
    (rect (- (/ sketch-width 2) 100) 5 200 50)
    (fill 0)
    (text (str x " : " y) (/ sketch-width 2) 15)
    (if (nil? selected)
      (text "nil" (/ sketch-width 2) 30)
      (if (empty? selected)
        (text "empty" (/ sketch-width 2) 30)
        (do
          (text (str ((first selected) "Title")) (/ sketch-width 2) 30)
          (text (str ((first selected) "Overall_Gross") "$") (/ sketch-width 2) 45))))
    (if (mouse-state)
      (swap! diagram-state assoc :selected (find-data x y (deref diagram-state))))))


(defn mouse-moved []
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))


#_(defsketch neo-noir-analysis-screen
  :title  "Einnahmen und Rankings im Neo-Noir-Genre"
  :setup setup
  :draw draw
  :mouse-moved mouse-moved
  :size [sketch-width sketch-height])
