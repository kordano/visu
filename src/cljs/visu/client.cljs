(ns visu.client
  (:require goog.net.WebSocket
            [goog.color :as gcolor]
            [dommy.core :as dom]
            [hiccups.runtime :as hiccupsrt]
            [cljs.reader :refer [read-string]]
            [clojure.browser.repl]
            [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants :as constants]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]
            [cljs.core.async :as async :refer [>! <! chan put! take! timeout close! map< map> filter< filter>]]
            [visu.artist :refer [clear-canvas draw-text draw-arc draw-line draw-rect generate-random-color rgb-to-string]]
            )
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
    :data nil
    :drawing true}))

(defn log [s]
  (.log js/console (str s)))

(set! clojure.core/*print-fn* (fn [& s] (.log js/console (apply str s))))

;; --- WEBSOCKET CONNECTION ---

(defn send! [data]
  (.send @websocket* (str data)))


(defn- take-all! [raw-message]
  (let [message (read-string raw-message)
        data (message :data)]
    (do
      (log (str "data received: " data))
      (swap! sketch-state assoc :data data))))


(defn client-connect! []
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
                          (take-all! data)))]]))
  (set! (.-onclick (sel1 :#disconnect-button)) (fn [] (.close @websocket*) (reset! websocket* nil)))
  (log "websocket loaded."))

;; --- DRAWING STUFF ---

(defn cleanup []
  (clear-canvas (deref sketch-state) (sel1 :#the-canvas))
  (swap! sketch-state assoc :drawing false))


(defn draw-cancer-graph []
  (let [raw-data (@sketch-state :data)
        canvas (sel1 :#the-canvas)
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
      (cleanup)
      (draw-text canvas (/ bar-width 2) 20 "Female Cancer Distribution" 16 "center" "#50afde")
      (doall
       (map
        #(let [child-size (- (* scale (the-children (val %))))
               mid-size (- (* scale (mid-adults (val %))))
               older-size (- (* scale (older-adults (val %))))
               x (+ 5 (* (key %) step))]
           (do
             (draw-rect
              canvas
              x y-start
              (- step 5) child-size
              "#BB66AA")
             (draw-rect
              canvas
              x (+ y-start child-size)
              (- step 5) mid-size
              "#AABB66")
             (draw-rect
              canvas
              x (+ y-start child-size mid-size)
              (- step 5) older-size
              "#66AABB")
             (draw-text canvas (+ x (/ step 2)) (+ y-start 20) (cancer-type (val %)) 14 "center" "#7070aa")))
        sorted-hashmap)))))


(defn draw-word [word x-position y-position words-list]
  (let [canvas (sel1 :#the-canvas)
        ctx (.getContext canvas "2d")
        meta-data (@sketch-state :meta-data)
        a (Math/sqrt (/ (- (val word) (meta-data :n-min))
                        (- (meta-data :n-max) (meta-data :n-min))))
        text-size (+
                   (* (- 1 a) (meta-data :text-min))
                   (* a (meta-data :text-max)))]
    (set! (.-font ctx) (str text-size "px Open Sans"))
    (set! (.-textAlign ctx) "left")
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
        (draw-word (first words-list) (+ x-start (.-width (.measureText ctx (key word)))) y-start (rest words-list))))))


(defn draw-word-cloud []
  (let [data (remove #(< (val %) 5) (@sketch-state :data))
        canvas (sel1 :#the-canvas)
        ctx (.getContext canvas "2d")]
    (swap! sketch-state assoc :meta-data
           {:n-max (apply max (vals data))
            :n-min 5
            :text-min 2
            :text-max 50})
    (cleanup)
    (set! (.-fillStyle ctx) "#202035")
    (.fillRect ctx 0 0 (@sketch-state :width) (@sketch-state :height))
    (draw-word (first data) 0 50 (rest data))))


(defn draw-spiral [theta r]
  (let [canvas (sel1 :#the-canvas)
        x (+ (/ (@sketch-state :width) 2) (* r (Math/cos theta)))
        y (+ (/ (@sketch-state :height) 2)(* r (Math/sin theta)))]
    (if (@sketch-state :drawing)
      (do
        (draw-arc canvas x y 1 0 (* 2 Math/PI) (rgb-to-string (generate-random-color [255 255 255])))
        (if (>= theta (* 256 Math/PI))
          (log "done")
          (go
            (<! (timeout 1))
            (draw-spiral (+ theta (* 3 (/ Math/PI 200))) (+ r 0.05)))))
      (println "Drawing interrupted ..."))))


(defn add-vectors [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn delta-vectors [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vector-length [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn scalar-mult-vector [k [x y]]
  [(* k x) (* k y)])

(defn prepare-graph-data [data]
  (let [edges (@sketch-state :data)
        vertices (->> @sketch-state
               :data
               (map #(into [] %))
               flatten
               (into #{})
               (map #(vector % {:position [(+ (/ (@sketch-state :width) 2) (* (rand 550) (Math/cos (rand (* 64 Math/PI)))))
                                           (+ (/ (@sketch-state :height) 2) (* (rand 350) (Math/sin (rand (* 64 Math/PI)))))]
                                :displacement [0 0]}))
               (into {}))]
    (swap! sketch-state assoc :data {:edges edges :vertices vertices :constants {:k (* 0.25 (Math/sqrt (/ (* (@sketch-state :width) (@sketch-state :height)) (count vertices))))}})))


(defn draw-force-based-graph []
  (let [data (-> @sketch-state :data)
        sketch-height (@sketch-state :height)
        sketch-width (@sketch-state :width)
        canvas (sel1 (@sketch-state :canvas))
        ctx (.getContext canvas "2d")
        vertices ()]
    (cleanup)
    (doall
     (map #(draw-arc canvas (-> % val :x) (-> % val :y) 3  0 (* 2 Math/PI) "#ffffff") data))))


;; --- HTML STUFF ---

(defn enable-buttons []
  (do
    (set! (.-onclick (sel1 :#connect-button)) (fn [] (client-connect!)))
    (set!
     (.-onclick (sel1 :#cancer-bar-graph-button))
     (fn [] (go
             (send! {:type "get" :data "cancer"})
             (<! (timeout 500))
             (draw-cancer-graph)
             (dom/set-text! (sel1 :#header-title) "Cancer Graph"))))
    (set!
     (.-onclick (sel1 :#word-cloud-button))
     (fn [] (go
             (send! {:type "get" :data "wordcloud"})
             (<! (timeout 500))
             (draw-word-cloud)
             (dom/set-text! (sel1 :#header-title) "Wordcloud"))))
    (set!
     (.-onclick (sel1 :#spiral-button))
     (fn [] (go
             (cleanup)
             (swap! sketch-state assoc :drawing true)
             (draw-spiral 0 0)
             (dom/set-text! (sel1 :#header-title) "Spiral"))))
    (set!
     (.-onclick (sel1 :#force-based-graph-button))
     (fn [] (go
             (send! {:type "get" :data "graph"})
             (<! (timeout 500))
             (cleanup)
             (swap! sketch-state assoc :drawing true)
             (prepare-graph-data (@sketch-state :data))
             (dom/set-text! (sel1 :#header-title) "Force based graph"))))
    (set!
     (.-onclick (sel1 :#real-3d-button))
     (fn [] (go
             (cleanup)
             (dom/set-text! (sel1 :#header-title) "3D on 2D, hot!"))))
    (set!
     (.-onclick (sel1 :#clear-canvas-button))
     (fn [] (cleanup)))))


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
         [:li [:a#force-based-graph-button "Force-based graph"]]
         [:li [:a#spiral-button "Spiral"]]
         [:li [:a#real-3d-button "No way, its 3D!!1"]]
         [:li [:a#clear-canvas-button "Clear"]]]]
       [:li.cat3 [:a#header-title "Title"]]]])))


(defn init []
  (let [body (sel1 :body)
        state (deref sketch-state)]
    (do
      (create-nav)
      (dom/append! body [:div#canvas-div [:canvas#the-canvas {:width (state :width) :height (state :height)}]])
      (dom/append! body [:script#fragment-shader {:type "x-shader/x-fragment"} "void main(void) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
  }" ])
      (dom/append! body [:script#vertex-shader {:type "x-shader/x-vertex"} "attribute vec3 aVertexPosition;

  uniform mat4 uMVMatrix;
  uniform mat4 uPMatrix;

  void main(void) {
    gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
  }"])
      (enable-buttons)
      (client-connect!))))


(set! (.-onload js/window) init)


;; live-coding stuff

#_(client-connect!)
#_(@sketch-state :drawing)
#_(send! {:type "get" :data "graph"})

#_(cleanup)



;; fruchterman-reingold stuff
#_(let [vertices (-> @sketch-state :data :vertices)
        edges (-> @sketch-state :data :edges)
        canvas (sel1 :#the-canvas)]
    (go
      (while true
        (<! (timeout 1))
        (doall
         (map #(draw-arc canvas (-> % val :position first) (-> % val :position second) 3 0 (* 2 Math/PI) "#ff2222") vertices))
        (doall
         (map #(draw-line canvas (-> % first vertices :position first) (-> % first vertices :position second) (-> % second vertices :position first) (-> % second vertices :position second) "#aaaaaa") edges)))))

#_(defn calculate-displacements []
     (let [vertices (-> @sketch-state :data :vertices)
           edges (-> @sketch-state :data :edges)
           k (-> @sketch-state :data :constants :k)]
       (do
         (map
          (fn [x]
            (swap! sketch-state assoc-in [:data :vertices x :displacement]
                   (reduce add-vectors
                           (map
                            #(scalar-mult-vector
                              (/ (* k k)
                                 (* (vector-length (delta-vectors ((vertices x) :position) ((vertices %) :position)))
                                    (vector-length (delta-vectors ((vertices x) :position) ((vertices %) :position)))))
                              (delta-vectors ((vertices x) :position) ((vertices %) :position)))
                            (remove #(= % x) (keys vertices))))))
          (keys vertices)))))

#_(calculate-displacements)

#_(let [vertices (-> @sketch-state :data :vertices)
           edges (-> @sketch-state :data :edges)
      k (-> @sketch-state :data :constants :k)]
    (map #(swap! sketch-state assoc-in [:data (vertices %) :position] (add-vectors ((vertices %) :position) ((vertices %) :displacement))) (keys vertices)))


;; Here comes the webgl or not

#_(def gl (context/get-context (sel1 :#the-canvas)))
#_(def fragment-shader (shaders/create-shader gl constants/fragment-shader (.-innerHTML (sel1 :#fragment-shader))))
#_(def vertex-shader (shaders/create-shader gl constants/vertex-shader (.-innerHTML (sel1 :#vertex-shader))))
#_(def shader-program (shaders/create-program gl [vertex-shader fragment-shader]))
#_(def triangle-buffer (buffers/create-buffer gl (ta/float32 [0.0 1.0 -1.0 -1.0 1.0 -1.0]) constants/array-buffer constants/static-draw))

#_(buffers/draw-arrays gl shader-program triangle-buffer constants/array-buffer
                         (shaders/get-attrib-location gl
                                                      shader-program
                                                      "vertex_position")
                       constants/triangles
                       constants/float
                       0
                       2
                       false
                       0
                       0
                       3
                       [{:name "color" :type :vec4 :values [1.0 0.0 0.0 1.0]}])

#_(shaders/get-attrib-location gl shader-program "vertex_position")

#_(println  (.getShaderInfoLog gl vertex-shader))
