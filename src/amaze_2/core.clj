(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as a :refer [<! >! <!! timeout chan alt! go]])
  (:import [java.util Random]))

(def ^:dynamic *cell-scale* 0.8)

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

(defn cell-coord-to-rect
  "Given the width and height in coordinates - give the rect in Quil coordinates (rect-style)"
  [[w h] [gw gh] [x y :as coord]]
  (let [rect-width (/ gw w)
        rect-height (/ gh h)
        x-pos (* x rect-width)
        y-pos (* y rect-height)]
    [x-pos y-pos rect-width rect-height]))
(defn scale-rect [scale [x y w h]]
  (let [new-w (* scale w)
        new-h (* scale h)
        diff-x (/ (- w new-w) 2)
        diff-y (/ (- h new-h) 2)]
    [(+ x diff-x) (+ y diff-y) new-w new-h]))

(defn get-fill-color [maze coord]
  (if (= coord (:current maze))
    [20 200 20]
    (if ((:visited maze) coord)
      [20 20 255]
      [255 255 255])))

(defn draw-state [{maze :maze}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 80)
  (let [to-rect #(scale-rect *cell-scale* (cell-coord-to-rect
                                  [(:width maze) (:height maze)]
                                  [(q/width) (q/height)] %))]
    (doseq [coord (for [x (range (:width maze))
                        y (range (:height maze))] [x y])]
      (q/with-fill (get-fill-color maze coord)
        (apply q/rect (to-rect coord))))))

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
  {:width width :height height :visited #{} :current nil})

;; Implement the maze-generator algo using a step function that will return the 'next' version of the maze.
;; An alternative to a recursive function (or loop) that will make it easier to push new versions of the maze
;; to the drawing from a function that 'drives' the algorithm by stepping through the 'collection'.

(defn select-any
  "Inefficient randomizer fn - select one element"
  [col]
  (nth col (-> (new Random) (.nextInt (count col)))))

(defn neighbours
  [w h [x y]]
  (let [pred (fn [[x y]]
               (and
                (and (>= x 0) (>= y 0))
                (and (< x w) (< y h))))]
    (filter pred ((juxt (partial mapv + [0 1])
                        (partial mapv + [1 0])
                        (partial mapv + [-1 0])
                        (partial mapv + [0 -1])) [x y]))))

(defn depth-first-maze-generator [{done :done
                                   visited :visited
                                   path :path
                                   current :current :as maze}]
  (if done maze ;; Return if we're done
      ;; Find not visited-cells of the current cell. Select a random, and move
      ;; there (update current and path) If there are none, pop :path and assign
      ;; to :current. If there is no path left - we're done.
      maze
      ))

