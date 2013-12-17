(ns visu.artist
  (:require goog.net.WebSocket
            [goog.color :as gcolor]
            [dommy.core :as dom]
            [hiccups.runtime :as hiccupsrt]
            [cljs.reader :refer [read-string]]
            [clojure.browser.repl]
            [cljs.core.async :as async :refer [>! <! chan put! take! timeout close! map< map> filter< filter>]])
  (:require-macros [hiccups.core :as hiccups]
                   [cljs.core.async.macros :refer [go alt!]]
                   [dommy.macros :refer [sel sel1 node deftemplate]]))



(defn generate-random-color [[r g b]]
  (let [r-new (rand-int 255)
        g-new (rand-int 255)
        b-new (rand-int 255)]
    [(/ (+ r r-new) 2)
     (/ (+ g g-new) 2)
     (/ (+ b b-new) 2)]))


(defn rgb-to-string [[r g b]]
  (gcolor/rgbToHex r g b))


(defn draw-rect [canvas x y w h c]
  (let [ctx (.getContext canvas "2d")]
    (do
      (set! (.-fillStyle ctx) c)
      (.fillRect ctx x y w h))))


(defn draw-arc [canvas x y r start-angle end-angle color]
  (let [ctx (.getContext canvas "2d")]
    (do
      (.beginPath ctx)
      (set! (.-strokeStyle ctx) color)
      (.arc ctx x y r start-angle end-angle)
      (.stroke ctx)
      )))


(defn draw-line [canvas x1 y1 x2 y2 color]
  (let [ctx (.getContext canvas "2d")]
    (.beginPath ctx)
    (set! (.-strokeStyle ctx) color)
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2)
    (.stroke ctx)))

(defn draw-text [canvas x y text size alignment color]
  (let [ctx (.getContext canvas "2d")]
    (do
      (set! (.-fillStyle ctx) color)
      (set! (.-font ctx) (str size "px Open Sans"))
      (set! (.-textAlign ctx) alignment)
      (.fillText ctx text x y))))


(defn clear-canvas [state canvas]
  ;; TODO: better state handling
  (let [ctx (.getContext canvas "2d")
        height (state :height)
        width (state :width)]
    (do
      (println "clearing canvas ...")
      (set! (.-fillStyle ctx) "#202035")
      (.fillRect ctx 0 0 width height))))
