(ns visu.week-one
  (:use quil.core))

(def random-numbers (map (fn [x] (ceil (rand 99))) (range 256)))

(defn plot-points [numbers ypos]
  (dorun
   (for [i numbers]
     (let [x (* i 8)]
       (ellipse x ypos 8 8)))))

(defn bar-graph [numbers y]
  (let [counts (frequencies numbers)]
    (dorun
     (for [i (keys counts)]
       (do
         (fill 255 (* (counts i) 30) 0)
         (rect (* i 8) y  8 (* (- (counts i)) 10)))))))


(defn setup []
  (size 800 800)
  (background 0)
  (smooth)
  (fill 255 40)
  (no-stroke)
  (plot-points random-numbers (/ (width) 2))
  (bar-graph random-numbers 600))


(defsketch some-random-thingies
  :title "some-random-thingie"
  :setup setup
  :size [800 800])

(let [counts (frequencies random-list-1)]
  (for [i (keys counts)]
    (dec (counts i))))
