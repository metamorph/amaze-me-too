(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as async :refer [chan]]))

(defn update-state [{maze :maze queue :queue :as state}] state)
(defn draw-state [{maze :maze queue :queue :as state}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240))

(defn maze-canvas [queue]
    (q/sketch
    :title "Lost?"
    :size [500 500]
    :setup #({:maze nil :queue queue})
    :update update-state
    :draw draw-state
    ;; :on-close #(async/close! queue)
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

;; Try another way of sending the maze to the canvas.
;; The initial state will be created with a channel that will be used to poll
;; mazes to generate.

