(ns visu.exercise.cancer-analysis
  (:require [clojure.string :refer [split]])
  (:use quil.core))


(def data-path "data/cancer_data.csv")


(defn read-data [path]
  (let [lines (split (slurp path) #"\n")]
    (map #(split % #",") lines)))


(defn clean-data [path]
  (let [read-file (read-data path)
        column-names (rest (first read-file))
        row-names (map first (rest read-file))
        raw-data (map rest (rest read-file))
        data (map (fn [x] (let [parsed-data (map #(Integer/parseInt %) x)]
                           (into {} (map vector column-names parsed-data))))
                  raw-data)]
    (into {} (map vector row-names data))))


(defn create-legend []
  (let [x-start 20
        y-start 20
        step 30
        width 100
        captions ["Older Adults" "Mid Adults" "Children"]
        colors [[150 0 0] [0 150 0] [0 0 150]]]
    (fill 255)
    (dorun
     (for [i (range 3)]
       (let [[r g b] (colors i)]
         (do
           (fill r g b)
           (rect x-start (+ y-start (* i step)) 100 step)
           (fill 255 230 0)
           (text (captions i) (+ x-start 5) (+ y-start (* i step) 20))))))))


(defn draw-scales []
  (let [scale (/ 600 35000)
        x-start 45
        y-start 760]
    (fill 0)
    (line x-start y-start x-start 150)
    (text-align :center)
    (text "[in 1000]" x-start 150)
    (dorun
     (for [i (range 8)]
       (do
         (line (- x-start 5) (- y-start (* scale (* i 5000))) (+ x-start 5) (- y-start (* scale (* i 5000))))
         (text-align :left)
         (text (str (* i 5)) (- x-start 20) (+ 5 (- y-start (* scale (* i 5000))))))))))


(defn overall-bar-graph [data]
  (let [summarized   (map #(reduce + (vals %)) (vals data))
        overall-sum  (reduce + summarized)
        values       (apply vector (map #(float (/ % overall-sum)) summarized))
        data-keys    (apply vector (keys data))
        ch           (apply vector (map #(% "Children")  (vals data)))
        mid-adults   (apply vector (map #(% "Mid Adults")  (vals data)))
        older-adults (apply vector (map #(% "Older Adults")  (vals data)))
        scale (/ 600 35000)
        y-start 760]
      (dorun
       (for [i (range 7)]
         (do
           (fill 0 0 150)
           (rect
            (+ 50 (* i 100)) y-start
            95 (- (* scale (ch i))))
           (fill 0 150 0)
           (rect
            (+ 50 (* i 100)) (- y-start (* scale (ch i)))
            95 (- (* scale (mid-adults i))))
           (fill 150 0 0)
           (rect
            (+ 50 (* i 100)) (- y-start (* scale (ch i)) (* scale (mid-adults i)))
            95 (- (* scale (older-adults i))))
           (fill 0)
           (text-align :center)
           (text (data-keys i) (+ 100 (* i 100)) (+ 20 y-start)))))))


(defn setup []
  (let [data (clean-data data-path)]
    (background 255 40)
    (smooth)
    (create-legend)
    (draw-scales)
    (no-stroke)
    (overall-bar-graph (clean-data data-path))))


#_(defsketch cancer-analysis-screen
  :title "Analyse von Krebsdaten"
  :setup setup
  :size [800 800])
