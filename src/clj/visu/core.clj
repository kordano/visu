(ns visu.core
  (:require [clojure.string :refer [split]]
            [visu.exercise.cancer-analysis :as cancer :refer [clean-data]]
            [visu.parser :as parser :refer [get-text-frequencies get-adjacency-list get-weather-data]]))


(defn get-data [name]
  (case name
    "cancer" (cancer/clean-data "data/cancer_data.csv")
    "wordcloud" (parser/get-text-frequencies "data/kommunistisches_manifest.txt")
    "graph" (parser/get-adjacency-list "data/graph.adjl")
    "weatherdata" (parser/get-weather-data "data/niederschlag.vtk")
    "data not found"))
