(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as a :refer [<! >! <!! timeout chan alt! go]]))

(defn next-maze
  "Take the next maze in the queue"
  [queue timeout]
  (first (a/alts!! [queue (a/timeout timeout)])))

(defn update-state
  "Put the next maze on queue in the maze-slot"
  [state]
  (if-let [maze (next-maze (:queue state) 1)] ;; Only update the maze-slot if there is a new entry on queue.
    (assoc state :maze maze)
    state))

(defn draw-state [{maze :maze}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 80)
  (q/text (str maze) 100 100 200 200)) ;; Dummy - write a String.

(defn maze-canvas [queue]
    (q/sketch
    :title "Lost?"
    :size [500 500]
    :setup (fn [] {:maze nil :queue queue})
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
