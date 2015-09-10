(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup [] {})
(defn update-state [state] [])

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240))

(defn draw-maze [m]
  (q/sketch
   :title "Lost?"
   :size [500 500]
   :setup setup
   :update update-state
   :draw draw-state
   :features [:keep-on-top]
   :middleware [m/fun-mode]))


(defn create-maze
  "Constructor for creating an initial maze with the given size"
  [width height]
  (let [maze {:width width :height height}
        coords (for [x (range 0 width), y (range 0 height)] [x y])]
    (assoc maze :cells (reduce #(assoc %1 %2 {}) {} coords))))
(defn get-maze-cell
  "Get the value of a cell in a maze"
  [maze coord]
  (get-in maze [:cells coord]))


;; --- Hack and Slash
(def the-maze (-> (create-maze 4 4)
                  (assoc-in [:cells [2 2]] {:current true})
                  (assoc-in [:cells [1 1]] {:visited true})
                  (assoc-in [:cells [1 2]] {:visited true})))
(draw-maze the-maze)
