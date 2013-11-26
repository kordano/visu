(ns visu.exercise.cinema-top-noir-analysis
  (:use quil.core)
  (:require [clojure.string :refer [split]]))

(def movies
  ["sin_city"
   "hollywoodland"
   "mulholland_drive"
   "the_black_dahlia"
   "the_man_who_wasnt_there"])

(defn create-input-path [s]
  (str "data/" s ".csv"))

(def sketch-width 1100)
(def sketch-height 700)


(def diagram-state
  (atom
   {:x-start 50
    :x-end nil
    :y-start nil
    :scale (/ (/ sketch-height 2) 90000000)
    :y-end 50
    :country-colors nil
    :color-countries nil
    :data nil}))


(defn read-data [path]
  (let [lines (split (slurp path) #"\n")]
    (map #(split % #",") lines)))


(defn prepare-data [data]
  (let [column-names (first data)
        raw-data (map (fn [x] (into {} (map vector column-names x))) (rest data))]
    (map #(assoc % "Total Gross" (Integer/parseInt (% "Total Gross"))) raw-data)))


(defn prepare-list [data-list]
  (into {} (map vector movies data-list)))


(defn generate-random-color [[r g b]]
  (let [r-new (rand-int 255)
        g-new (rand-int 255)
        b-new (rand-int 255)]
    [(/ (+ r r-new) 2)
     (/ (+ g g-new) 2)
     (/ (+ b b-new) 2)]))


(defn draw-pie-chart [x y state movie]
  (let [data ((state :data) movie)
        country-colors (state :country-colors)
        gross (apply vector (map #(% "Total Gross") data))
        world-gross (reduce + gross)
        scale (/ (* 2 PI) world-gross)
        radians (apply vector (map #(* scale %) gross))
        countries (apply vector (map #(% "Country") data))
        pie-size (/ sketch-height 3)]
    (dorun
     (for [i (range (count data))]
       (let [new-color (generate-random-color [255 255 255])
             radian-sum (reduce + (take i radians))]
         (do
           (apply fill (country-colors (countries i)))
           (arc x y pie-size pie-size radian-sum (+ radian-sum (radians i)))))))
    (text-align :center)
    (fill 0)
    (text movie x 50))
  state)


(defn draw-charts [state]
  (do
    (draw-pie-chart (/ sketch-width 6) (/ sketch-height 3) state "sin_city")
    (draw-pie-chart (/ sketch-width 3) (* 2 (/ sketch-height 3)) state "hollywoodland")
    (draw-pie-chart (/ sketch-width 2) (/ sketch-height 3) state "the_black_dahlia")
    (draw-pie-chart (* 2 (/ sketch-width 3)) (* 2 (/ sketch-height 3)) state "mulholland_drive")
    (draw-pie-chart (* 5 (/ sketch-width 6)) (/ sketch-height 3) state "the_man_who_wasnt_there"))
  state)


(defn draw-legend [state]
  (let [countries (apply vector (keys (state :country-colors)))
        colors (apply vector (vals (state :country-colors)))]
    (dorun
     (for [i (range (count countries))]
       (do
         (apply fill (colors i))
         (rect sketch-width (+ 50 (* i 15)) (- 9) (- 9))
         (fill 0)
         (text-align :left)
         (text (countries i) (+ sketch-width 10) (+ 50 (* i 15)))))))
  state)


(defn setup []
  (let [data-list (map #(prepare-data (read-data (create-input-path %))) movies)
        countries (into #{} (flatten (map (fn [m] (map #(% "Country") m)) data-list)))
        country-colors (map #(vector % (generate-random-color [255 255 255])) countries)]
    (swap! diagram-state assoc
           :data (prepare-list data-list)
           :country-colors (into {} country-colors)
           :color-countries (into {} (map #(vector (apply color (second %)) (first %)) country-colors)))
    (background 255 40)
    (no-stroke)
    (set-state! :mouse-position (atom [0 0]))
    (-> (deref diagram-state)
        draw-charts
        draw-legend)))


(defn draw []
  (let [[x y] @(state :mouse-position)
        colors ((deref diagram-state) :color-countries)]
    (fill 255)
    (no-stroke)
    (rect (- (/ sketch-width 2) 100) (- sketch-height 50) 200 60)
    (fill 0)
    (text-align :center)
    (text (str (colors (get-pixel x y)) ) (/ sketch-width 2) (- sketch-height 20))))


(defn mouse-moved []
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))


(defsketch top-noir
  :title "Analyse der Top Noir Film"
  :setup setup
  :draw draw
  :mouse-moved mouse-moved
  :size [(+ sketch-width 200) sketch-height])
