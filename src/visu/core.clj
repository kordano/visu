(ns visu.core
  (:use quil.core)
  (:require [visu.dynamic :as dynamic]
            [visu.curves :as curves]))


(defsketch bubbles
  :title "I love bubbles!"
  :setup dynamic/setup
  :draw dynamic/draw
  :mouse-released dynamic/mouse-released
  :size [600 500])


(defsketch more-curves
  :title "curves"
  :setup curves/setup
  :draw curves/draw
  :size [600 500])
