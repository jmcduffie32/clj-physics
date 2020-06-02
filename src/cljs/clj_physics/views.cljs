(ns clj-physics.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [clj-physics.subs :as subs]))

(defonce interval (r/atom nil))
(defonce state
  (r/atom {:particles [{:x 0 :y 0
                        :vx 1 :vy 1}
                       {:x 10 :y 0
                        :vx 1 :vy 1}
                       {:x 11 :y 5
                        :vx 1 :vy 1}
                       {:x 0 :y 4
                        :vx 1 :vy 1}
                       {:x 30 :y 5
                        :vx 1 :vy 1}]
           :max-x 100
           :max-y 100}))


(defn clear-canvas [canvas]
  (let [ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (set! (.-fillStyle ctx) "white")
    (doto ctx
      (.beginPath)
      (.rect 0 0 w h)
      (.fill))))

(defn get-new-y [y vy]
  (if (> y (:max-y @state))
    {:y (- y vy) :vy (* -1 vy)}
    (if (< y 0)
      {:y (- y vy) :vy (* -1 vy)}
      {:y (+ vy y) :vy vy})))

(defn get-new-x [x vx]
  (if (> x (:max-x @state))
    {:x (- x vx) :vx (* -1 vx)}
    (if (< x 0)
      {:x (- x vx) :vx (* -1 vx)}
      {:x (+ vx x) :vx vx})))

(defn update-particles [state]
  (assoc state :particles
         (mapv (fn [{:keys [x y vx vy]}]
                 (let [{:keys [x vx]} (get-new-x x vx)
                       {:keys [y vy]} (get-new-y y vy)]
                   {:x x :vx vx :y y :vy vy}))
               (:particles state))))

(defn update-world []
  (swap! state update-particles)
  (println "Updating positions"))

(defn paint [canvas ctx]
  (let [particles (:particles @state)]
    (println "Painting the screen")
    (clear-canvas canvas)
    (doseq [{:keys [x y vx vy]} particles]
      (println x y vx vy)
      (set! (.-fillStyle ctx) "black")
      (doto ctx
        (.beginPath)
        (.rect x y 2 2)
        (.fill)))))

(defn tick [canvas ctx]
  (update-world)
  (paint canvas ctx))

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
      [:canvas#drawing1])}))

(defn home-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1 (str "Hello from " @name ". This is the Home Page.")]

     [:div
      [:a {:href "#/about"}
       "go to About Page"]]

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
