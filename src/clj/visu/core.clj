(ns visu.core
  (:require [clojure.string :refer [split]]
            [visu.exercise.cancer-analysis :as cancer :refer [clean-data]]
            [visu.parser :as parser :refer [get-text-frequencies]]))


(defn get-data [name]
  (condp = name
    "cancer" (cancer/clean-data "data/cancer_data.csv")
    "manifesto" (parser/get-text-frequencies "data/kommunistisches_manifest.txt")
    "english_stopwords" (split (slurp "data/english_stopwords.txt") #",")
    "data not found"))
