(ns visu.client
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
  (set! (.-onclick (sel1 :#disconnect-button)) (fn [] (.close @websocket*) (reset! websocket* nil)))
  (log "websocket loaded."))

;; --- CANVAS STUFF ---

(defn generate-random-color [[r g b]]
  (let [r-new (rand-int 255)
        g-new (rand-int 255)
        b-new (rand-int 255)]
    [(/ (+ r r-new) 2)
     (/ (+ g g-new) 2)
     (/ (+ b b-new) 2)]))


(defn rgb-to-string [[r g b]]
  (gcolor/rgbToHex r g b))


(defn draw-rect [x y w h c]
  (let [ctx (.getContext (sel1 :#the-canvas) "2d")]
    (do
      (set! (.-fillStyle ctx) c)
      (.fillRect ctx x y w h))))


(defn draw-text [x y text size alignment color]
  (let [ctx (.getContext (sel1 :#the-canvas) "2d")]
    (do
      (set! (.-fillStyle ctx) color)
      (set! (.-font ctx) (str size "px Open Sans"))
      (set! (.-textAlign ctx) alignment)
      (log (.-textAlign ctx))
      (.fillText ctx text x y))))

(defn clear-canvas []
  (let [ctx (.getContext (sel1 :#the-canvas) "2d")
        height (@sketch-state :height)
        width (@sketch-state :width)]
    (do
      (log "clearing canvas ...")
      (set! (.-fillStyle ctx) "#202035")
      (.fillRect ctx 0 0 width height))))

(defn draw-cancer-graph []
  (let [raw-data (@sketch-state :data)
        data (vals raw-data)
        cancer-type (apply vector (keys raw-data))
        scale (/ 400.0 35000)
        y-start 450
        bar-width 600
        step (/ 600 (count data))
        the-children (apply vector (map #(% "Children") data))
        mid-adults (apply vector (map #(% "Mid Adults") data))
        older-adults (apply vector (map #(% "Older Adults") data))
        summarized (apply vector (map #(reduce + (vals %)) data))
        sorted-summarized (->> (map vector (range (count data)) summarized)
                              (into {})
                              (sort-by val)
                              keys
                              reverse)
        sorted-hashmap (into {} (map vector (range (count data)) sorted-summarized))]
    (do
      (clear-canvas)
      (draw-text (/ bar-width 2) 20 "Female Cancer Distribution" 16 "center" "#50afde")
      (doall
       (map
        #(let [child-size (- (* scale (the-children (val %))))
               mid-size (- (* scale (mid-adults (val %))))
               older-size (- (* scale (older-adults (val %))))
               x (+ 5 (* (key %) step))]
           (do
             (draw-rect
              x y-start
              (- step 5) child-size
              "#BB66AA")
             (draw-rect
              x (+ y-start child-size)
              (- step 5) mid-size
              "#AABB66")
             (draw-rect
              x (+ y-start child-size mid-size)
              (- step 5) older-size
              "#66AABB")
             (draw-text (+ x (/ step 2)) (+ y-start 20) (cancer-type (val %)) 14 "center" "#7070aa")))
        sorted-hashmap)))))

(defn draw-word [word x-position y-position words-list]
  (let [ctx (.getContext (sel1 :#the-canvas) "2d")
        meta-data (@sketch-state :meta-data)
        a (Math/sqrt (/ (- (val word) (meta-data :n-min))
                        (- (meta-data :n-max) (meta-data :n-min))))
        text-size (+
                   (* (- 1 a) (meta-data :text-min))
                   (* a (meta-data :text-max)))]
    (set! (.-font ctx) (str text-size "px Open Sans"))
    (set! (.-fillStyle ctx) "#66AABB")
    (let [x-start (if (< 1100 (+ x-position (.-width (.measureText ctx (key word)))))
                    0
                    x-position)
          y-start (if (< 1100 (+ x-position (.-width (.measureText ctx (key word)))))
                    (+ y-position (meta-data :text-max))
                    y-position)]
      (.fillText ctx (key word) x-start y-start)
      (if-not (seq words-list)
        (log "done")
        (recur (first words-list) (+ x-start (.-width (.measureText ctx (key word)))) y-start (rest words-list))))))


(defn draw-word-cloud []
  (let [data (remove #(< (val %) 5) (@sketch-state :data))
        ctx (.getContext (sel1 :#the-canvas) "2d")]
    (swap! sketch-state assoc :meta-data
           {:n-max (apply max (vals data))
            :n-min 5
            :text-min 2
            :text-max 50})
    (clear-canvas)
    (set! (.-fillStyle ctx) "#202035")
    (.fillRect ctx 0 0 (@sketch-state :width) (@sketch-state :height))
    (draw-word (first data) 0 50 (rest data))))


(defn draw-force-based-graph []
  (let [data ((@sketch-state :data))
        ctx (.getContext (sel1 :#the-canvas) "2d")]
    (clear-canvas)))


;; --- HTML STUFF ---

(defn enable-buttons []
  (do
    (set! (.-onclick (sel1 :#connect-button)) (fn [] (establish-websocket)))
    (set!
     (.-onclick (sel1 :#cancer-bar-graph-button))
     (fn [] (go
             (send-data {:type "get" :data "cancer"})
             (<! (timeout 500))
             (draw-cancer-graph)
             (dom/set-text! (sel1 :#header-title) "Cancer Graph"))))
    (set!
     (.-onclick (sel1 :#word-cloud-button))
     (fn [] (go
             (send-data {:type "get" :data "wordcloud"})
             (<! (timeout 500))
             (draw-word-cloud)
             (dom/set-text! (sel1 :#header-title) "Wordcloud"))))
    (set!
     (.-onclick (sel1 :#clear-canvas-button))
     (fn [] (clear-canvas)))))


(defn create-nav []
  (let [body (sel1 :body)]
    (dom/append!
     body
     [:nav
      [:ul
       [:li.cat1 [:a  "Home"]
        [:ul
         [:li [:a#connect-button  "Connect"]]
         [:li [:a#disconnect-button "Disconnect"]]]]
       [:li.cat2
        [:a  "Drawings"]
        [:ul
         [:li [:a#cancer-bar-graph-button  "Cancer bar graph"]]
         [:li [:a#word-cloud-button "Word Cloud"]]
         [:li [:a#force-based-graph "Force-based graph"]]
         [:li [:a#clear-canvas-button "Clear"]]]]
       [:li.cat3 [:a#header-title "Title"]]]])))


(defn init []
  (let [body (sel1 :body)
        state (deref sketch-state)]
    (do
      (create-nav)
      (dom/append! body [:div#canvas-div [:canvas#the-canvas {:width (state :width) :height (state :height)}]])
      (enable-buttons)
      (establish-websocket))))


(set! (.-onload js/window) init)

#_(init)
#_(establish-websocket)
#_(clear-canvas)
#_(send-data {:type "get" :data "wordcloud"})
#_(draw-word-cloud)
#_(create-nav)
