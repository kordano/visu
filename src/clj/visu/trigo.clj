(ns visu.trigo
  (:use quil.core))

(def world (atom {:radius 50}))

(defn setup []
  (size 600 200)
  (background 127)
  (text-font (create-font "Verdana" 12)))


(defn draw []
  (background 127)
  (no-stroke)
  (fill 255)
  (ellipse (/ (width) 8) 75 (* (:radius @world) 2) (* (:radius @world) 2)))
