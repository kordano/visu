(ns visu.parser
  (:refer-clojure :exclude [replace])
  (:require [clojure.core :as core]
            [clojure.string :refer [split replace blank? lower-case]]))

(def special-characters [";" "," "\\." "!" "\\?" ":" "\\*" "\""  "'" "_" "-" "\\)" "\\(" "\r" "\\]" "\\["])

(defn- remove-special-chars [word special-chars-list]
  (if-not (seq special-chars-list)
    (lower-case word)
    (recur (replace word (re-pattern (first special-chars-list)) "") (rest special-chars-list))))

(defn get-text-frequencies [path]
  (let [lines (split (slurp "data/kommunistisches_manifest.txt") #"\n")
        raw-words (flatten (map #(split % #" ") lines))
        special-characters-removed (map #(remove-special-chars % special-characters) raw-words)]
    (frequencies (remove blank? special-characters-removed))))
