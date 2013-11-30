(ns visu.client
  (:require goog.net.WebSocket
            [dommy.core :as dom]
            [hiccups.runtime :as hiccupsrt]
            [cljs.reader :refer [read-string]]
            [clojure.browser.repl])
  (:require-macros [hiccups.core :as hiccups]
                   [dommy.macros :refer [sel sel1 node deftemplate]]))

;; fire up repl
#_(do
    (def repl-env (reset! cemerick.austin.repls/browser-repl-env
                          (cemerick.austin/repl-env)))
    (cemerick.austin.repls/cljs-repl repl-env))

(def websocket* (atom nil))

(defn log [s]
  (.log js/console (str s)))

(defn send-data [data]
  (.send @websocket* (str data)))

(defn- receive-data [raw-message]
  (let [message (read-string raw-message)
        data (message :data)]
    (log data)))

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


(def sketch-state
  (atom
   {:width 1100
    :height 600}))

(defn log [s]
  (.log js/console (str s)))

(defn draw []
  (let [canvas (sel1 :#theCanvas)
        ctx (.getContext canvas "2d")]
    (do
      (set! (.-globalCompositeOperation ctx) "source-over")
      (set! (.-fillStyle ctx) "rgba(255,255,255,0.5)")
      (.strokeRect ctx 100 100 50 50))))

(defn init []
  (let [body (sel1 :body)
        state (deref sketch-state)]
    (do
      (dom/append!
       body
       [:canvas#theCanvas
        {:width (state :width) :height (state :height)}])
      (draw))))

(set! (.-onload js/window) init)

#_(establish-websocket)
#_(send-data {:type "get-data" :data "cancer"})
