(ns clj-physics.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [clj-physics.subs :as subs]))

(defonce interval (r/atom nil))
(defonce state
  (r/atom {:particles [{:x 0 :y 0
                        :vx 2 :vy 1}]
           :delta-t 0}))
(def canvas-height 150)
(def canvas-width 300)


(defn clear-canvas [canvas]
  (let [ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (set! (.-fillStyle ctx) "white")
    (doto ctx
      (.beginPath)
      (.rect 0 0 w h)
      (.fill))))

(defn delta-y [vy delta-t]
  (+ (/ (* delta-t delta-t -1) 2)
     (* vy delta-t)))

(defn delta-x [vx delta-t]
  (* vx (rem delta-t 100)))

(defn update-particles [state]
  (-> state
      (update :delta-t (fn [t] (+ t (/ 1 60))))
      (assoc :particles
             (mapv (fn [{:keys [vx vy]}]
                     (let [delta-t (:delta-t state)
                           x (delta-x vx delta-t)
                           y (delta-y vy delta-t)]
                       {:x x :vx vx :y y :vy vy}))
                   (:particles state)))))

(defn update-world []
  (swap! state update-particles))

(defn draw-line [ctx [x1 y1] [x2 y2]]
  (doto ctx
    (.beginPath)
    (.moveTo x1 y1)
    (.lineTo x2 y2)
    (.stroke)))

(defn draw-axes [ctx]
  (draw-line ctx
             [0 canvas-height]
             [0 0])
  (draw-line ctx
             [0 canvas-height]
             [canvas-width canvas-height]))

(defn paint [canvas ctx]
  (let [particles (:particles @state)]
    (clear-canvas canvas)
    (draw-axes ctx)
    (doseq [{:keys [x y]} particles]
      (set! (.-fillStyle ctx) "black")
      (doto ctx
        (.beginPath)
        (.rect x (- canvas-height y) 2 2)
        (.fill)))))

(defn tick [canvas ctx]
  (update-world)
  (paint canvas ctx))

(defn reset-world [state]
  (assoc state :delta-t 0))

(defn canvas-page []
  (r/create-class
   {:display-name "canvas-page"
    :component-did-mount
    (fn [this]
      (let [canvas (rdom/dom-node this)
            ctx (.getContext canvas "2d")]
        (if (not (nil? @interval))
          (js/clearInterval @interval))
        (reset! interval (js/setInterval #(tick canvas ctx) (/ 1000 60)))))
    :reagent-render
    (fn []
      [:canvas#drawing1 {:style {:height canvas-height
                                 :width  canvas-width}}])}))

(defn home-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1 (str "Hello from " @name ". This is the Home Page.")]

     [:div
      [:a {:href "#/about"}
       "go to About Page"]]

     [:div
      [:label
       "vy0"
       [:input {:type "number"
                :value (get-in @state [:particles 0 :vy])
                :on-change #(swap! state assoc-in [:particles 0 :vy]
                                   (-> % .-target .-value))}]]
      [:label
       "vx0"
       [:input {:type "number"
                :value (get-in @state [:particles 0 :vx])
                :on-change #(swap! state assoc-in [:particles 0 :vx]
                                   (-> % .-target .-value))}]]
      [:label
       "delta t"
       [:input {:type "number"
                :readonly true
                :value (.toFixed (get @state :delta-t) 2)}]]
      [:button
       {:on-click #(swap! state reset-world)}
       "set t = 0"]]
     [canvas-page]]))


;; about

(defn about-panel []
  [:div
   [:h1 "This is the About Page."]

   [:div
    [:a {:href "#/"}
     "go to Home Page"]]])


;; main

(defn- panels [panel-name]
  (case panel-name
    :home-panel [home-panel]
    :about-panel [about-panel]
    [:div]))

(defn show-panel [panel-name]
  [panels panel-name])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [show-panel @active-panel]))
