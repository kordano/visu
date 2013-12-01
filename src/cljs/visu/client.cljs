(ns visu.client
  (:require goog.net.WebSocket
            [dommy.core :as dom]
            [hiccups.runtime :as hiccupsrt]
            [cljs.reader :refer [read-string]]
            [clojure.browser.repl]
            [cljs.core.async :as async :refer [>! <! chan put! take! timeout close! map< map> filter< filter>]])
  (:require-macros [hiccups.core :as hiccups]
                   [cljs.core.async.macros :refer [go alt!]]
                   [dommy.macros :refer [sel sel1 node deftemplate]]))

;; fire up repl
#_(do
    (def repl-env (reset! cemerick.austin.repls/browser-repl-env
                          (cemerick.austin/repl-env)))
    (cemerick.austin.repls/cljs-repl repl-env))

(def websocket* (atom nil))

(def sketch-state
  (atom
   {:width 1100
    :height 700
    :data nil}))

(defn log [s]
  (.log js/console (str s)))


;; --- WEBSOCKET CONNECTION ---

(defn send-data [data]
  (.send @websocket* (str data)))


(defn- receive-data [raw-message]
  (let [message (read-string raw-message)
        data (message :data)]
    (do
      (log (str "data received: " data))
      (swap! sketch-state assoc :data data))))

(defn establish-websocket []
  (log "establishing websocket ...")
  (reset! websocket* (js/WebSocket. "ws://localhost:9090"))
  (doall
   (map #(aset @websocket* (first %) (second %))
        [["onopen" (fn [] (do
                           (log "channel opened")
                           (.send @websocket* {:type "greeting" :data []})))]
         ["onclose" (fn [] (log "channel closed"))]
         ["onerror" (fn [e] (log (str "ERROR:" e)))]
         ["onmessage" (fn [m]
                        (let [data (.-data m)]
                          (receive-data data)))]]))
  (log "websocket loaded."))

#_(send-data {:type "get" :data "asd"})

;; --- CANVAS STUFF ---

(defn draw-rect [x y w h c]
  (let [ctx (.getContext (sel1 :#the-canvas) "2d")]
    (do
      (set! (.-fillStyle ctx) c)
      (.fillRect ctx x y w h))))


(defn clear-canvas []
  (let [ctx (.getContext (sel1 :#the-canvas) "2d")
        height (@sketch-state :height)
        width (@sketch-state :width)]
    (do
      (log "clearing canvas ...")
      (set! (.-fillStyle ctx) "#FFFFFF")
      (.fillRect ctx 0 0 width height))))

(defn draw-cancer-graph []
  (let [state (deref sketch-state)
        data (state :data)
        scale (/ 650.0 35000)
        the-children (apply vector (map #(% "Children") (vals data)))
        mid-adults (apply vector (map #(% "Mid Adults") (vals data)))
        older-adults (apply vector (map #(% "Older Adults") (vals data)))
        summarized (apply vector (map #(reduce + (vals %)) (vals data)))
        sorted-summarized (sort-by val (into {} (map vector (range (count (vals data))) summarized)))]
    (do
      (doall
       (map
        #(let [child-size (- (* scale (the-children %)))
               mid-size (- (* scale (mid-adults %)))
               older-size (- (* scale (older-adults %)))]
           (do
             (draw-rect
              (+ 5 (* % 50))
              650
              45
              child-size
              "#FF0000")
             (draw-rect
              (+ 5 (* % 50))
              (+ 650 child-size)
              45
              mid-size
              "#00FF00")
             (draw-rect
              (+ 5 (* % 50))
              (+ 650 child-size mid-size)
              45
              older-size
              "#0000FF")))
        (keys sorted-summarized))))))



;; --- HTML STUFF ---

(defn enable-buttons []
  (do
    (set! (.-onclick (sel1 :#connect-button)) (fn [] (establish-websocket)))
    (set! (.-onclick (sel1 :#cancer-graph-button)) (fn [] (go
                                                           (send-data {:type "get" :data "cancer"})
                                                           (<! (timeout 500))
                                                           (draw-cancer-graph))))
    (set! (.-onclick (sel1 :#clear-canvas-button)) (fn [] (clear-canvas)))))


(defn init []
  (let [body (sel1 :body)
        state (deref sketch-state)]
    (do
      (dom/append! body [:div#canvas-div [:canvas#the-canvas {:width (state :width) :height (state :height)}]])
      (dom/append! body [:button#connect-button {:type "button"} "Connect"])
      (dom/append! body [:button#disconnect-button {:type "button"} "Disconnect"])
      (dom/append! body [:button#clear-canvas-button {:type "button"} "Clear canvas"])
      (dom/append! body [:button#cancer-graph-button {:type "button"} "Draw cancer graph"])
      (enable-buttons))))



(set! (.-onload js/window) init)
#_(init)
#_(clear-canvas)
