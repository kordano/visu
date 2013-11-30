(ns visu.core
  (:require [visu.exercise.cancer-analysis :as cancer :refer [clean-data]]))


(defn get-data [name]
  (condp = name
    "cancer" (cancer/clean-data "data/cancer_data.csv")
    "data not found"))
