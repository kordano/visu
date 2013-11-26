(ns visu.transform
  (:use quil.core))


(defn setup []
  (size 200 200)
  (background 255)
  (no-stroke))


(defn draw []
  (fill 192)
  (rect 20 20 40 40)
  (fill 255 0 0 128)
  (rect (+ 20 60) (+ 20 80) 40 40)
  (fill 0 0 255 128)
  (push-matrix)
  (translate 160 180)
  (rect 20 20 40 40)
  (pop-matrix))

(defsketch transform-example
  :title "some transformations"
  :setup setup
  :draw draw
  :size [600 500])
