(ns visu.curves
  (:use quil.core))


(defn setup []
  (size 300 200)
  (background 255)
  (smooth))


(defn draw-arcs []
  (rect-mode :center)
  (stroke 128)
  (rect 35 35 50 50)
  (rect 105 35 50 50)
  (rect 175 35 50 50)
  (rect 105 105 100 50)
  (stroke 0)
  (arc 35 35 50 50 0 HALF-PI)
  (arc 105 35 50 50 (- PI) 0)
  (arc 175 35 50 50 (/ (- PI) 6) (/ PI 6))
  (arc 105 105 100 50 HALF-PI (* 3 HALF-PI)))


(defn draw-beziers []
  (ellipse 50 75 5 5)
  (ellipse 100 75 5 5)
  (fill 255 0 0)
  (ellipse 25 25 5 5)
  (ellipse 125 25 5 5)
  (no-fill)
  (stroke 0)
  (bezier 50 75 25 25 125 25 100 75))


(defn draw-continous-bezier []
  (begin-shape)
  (vertex 30 70)
  (bezier-vertex 25 25 100 50 50 100)
  (bezier-vertex 20 130 75 140 120 120)
  (end-shape))

(defn draw []
  (draw-beziers)
  (draw-continous-bezier)
  (fill 0)
  (text "bla" 200 200))


(defsketch more-curves
  :title "curves"
  :setup setup
  :draw draw
  :size [600 500])
