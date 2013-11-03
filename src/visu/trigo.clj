(ns visu.trigo
  (:use quil.core))


(defn setup []
  (size 600 200)
  (background 127)
  (text-font (create-font "Verdana" 12)))


(defn draw []
  (background 127)
  (no-stroke)
  (fill 255)
  (ellipse (/ width 8) 75 (* radius 2) (* radius 2)))
