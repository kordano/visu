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


(defn bar-graph [data]
  (let [summarized   (map #(reduce + (vals %)) (vals data))
        overall-sum  (reduce + summarized)
        values       (apply vector (map #(float (/ % overall-sum)) summarized))
        data-keys    (apply vector (keys data))
        ch           (apply vector (map #(float (/ (% "Children") overall-sum)) (vals data)))
        mid-adults   (apply vector (map #(float (/ (% "Mid Adults") overall-sum)) (vals data)))
        older-adults (apply vector (map #(float (/ (% "Older Adults") overall-sum)) (vals data)))
        scale        1000
        y-start      760]
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
    (size 800 800)
    (background 255 40)
    (smooth)
    (no-stroke)
    (bar-graph (clean-data data-path))))


(defsketch cancer-analysis-screen
  :title "Analyse von Krebsdaten"
  :setup setup
  :draw draw
  :size [800 800])
