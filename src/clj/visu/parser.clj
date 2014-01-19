(ns visu.parser
  (:refer-clojure :exclude [replace])
  (:require [clojure.core :as core]
            [clojure.string :refer [split replace blank? lower-case ]]))

(def special-characters [";" "," "\\." "!" "\\?" ":" "\\*" "\""  "'" "_" "-" "\\)" "\\(" "\r" "\\]" "\\["])
(def stopwords (into #{} (split (slurp "data/english_stopwords.txt") #",")))

(defn- remove-special-chars [word special-chars-list]
  (if-not (seq special-chars-list)
    (lower-case word)
    (recur (replace word (re-pattern (first special-chars-list)) "") (rest special-chars-list))))


(defn get-text-frequencies [path]
  (->> (split (slurp path) #"\n")
       (map #(split % #" "))
       flatten
       (map #(remove-special-chars % special-characters))
       (remove blank?)
       (remove stopwords)
       frequencies))


(defn get-adjacency-list [path]
  "Parse adjacency list and convert to readable form"
  (let [raw-data (split (slurp path) #"\n")]
    (->> (map #(split % #" ") raw-data)
         (map vector (map str (range (count raw-data))))
         (into {})
         (map (fn [x] (map #(into #{} [(key x) %]) (val x))))
         flatten
         (into #{}))))

(defn check-annotation [line]
  "parse vtk data"
  (let [possible-annotations ["X_COORDINATES" "Y_COORDINATES" "Z_COORDINATES" "LOOKUP_TABLE"]]
    (first (remove nil? (map #(re-find (re-pattern %) line) possible-annotations)))))


(defn parse-raw-vtk-data [path]
  (let [data (split (slurp path) #"\n")]
    (loop [line (first data)
           raw-coll (rest data)
           coll {}]
      (case (check-annotation line)
        "X_COORDINATES" (recur
                         (first raw-coll)
                         (rest raw-coll)
                         (into coll
                               [[:x-coordinates
                                 (map read-string (split (first raw-coll) #" "))]]))
        "Y_COORDINATES" (recur
                         (first raw-coll)
                         (rest raw-coll)
                         (into coll
                               [[:y-coordinates
                                 (map read-string (split (first raw-coll) #" "))]]))
        "Z_COORDINATES" (recur
                         (first raw-coll)
                         (rest raw-coll)
                         (into coll [[:z-coordinates (split (first raw-coll) #" ")]]))
        "LOOKUP_TABLE" (into coll [[:scalars (map read-string raw-coll)]])
        nil (recur (first raw-coll) (rest raw-coll) coll)))))


(defn create-point-list [vtk-data]
  (->> (map (fn [x]
              (map #(into {} [[:x x] [:y %]]) (:y-coordinates vtk-data)))
            (:x-coordinates vtk-data))
       flatten
       (apply vector)))


(defn create-cell-list [points values x-dim y-dim]
  (->> (map (fn [y]
              (map
               #(let [index1 (+ % (* y x-dim))
                      index2 (inc (+ % (* y x-dim)))
                      index3 (+ % (* (inc y) x-dim))
                      index4 (inc (+ % (* (inc y) x-dim)))]
                  (into {} [[:p1 (points index1)]
                            [:p2 (points index2)]
                            [:p3 (points index3)]
                            [:p4 (points index4)]
                            [:value (/ (+ (values index1) (values index2) (values index3) (values index4)) 4)]]))
               (range (dec x-dim))))
            (range (dec y-dim)))
       flatten
       (apply vector)))


(defn get-weather-data [path]
  (let [vtk-data (parse-raw-vtk-data path)
        x-dim (-> vtk-data :x-coordinates count)
        y-dim (-> vtk-data :y-coordinates count)
        points (create-point-list vtk-data)
        cells (create-cell-list points (apply vector (:scalars vtk-data)) x-dim y-dim)]
    {:points points
     :cells cells
     :scalars (:scalars vtk-data)
     :x-dim x-dim
     :y-dim y-dim}))


(->> (get-weather-data "data/niederschlag.vtk")
     :cells
     (map :value))
