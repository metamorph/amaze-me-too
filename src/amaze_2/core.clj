(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as a :refer [<! >! <!! timeout chan alt! go]])
  (:import [java.util Random]))

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
(defn cell-to-point [[w h :as size] [gw gh :as gsize] [x y :as coord]]
  (let [[x y rw rh] (cell-coord-to-rect size gsize coord)]
    [(+ x (/ rw 2)) (+ y (/ rh 2))]))

(defn draw-state [{maze :maze}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 80)
  (q/ellipse-mode :corner)
  (let [to-rect #(scale-rect %2 (cell-coord-to-rect
                                  [(:width maze) (:height maze)]
                                  [(q/width) (q/height)] %1))
        to-point #(cell-to-point [(:width maze) (:height maze)]
                                 [(q/width) (q/height)] %)]
    (q/stroke-weight 1)
    (doseq [coord (for [x (range (:width maze))
                        y (range (:height maze))] [x y])]
      (q/with-fill [255 255 255] (q/with-stroke [150 150 150]
          (apply q/rect (to-rect coord 1)))))
    (q/with-fill [200 255 200]
      (apply q/rect (to-rect (:current maze) 1) ))
    (q/with-fill [255 200 200]
      (doseq [p (:active maze)]
        (apply q/rect (to-rect p 1))))
    (q/with-stroke [20 20 20]
      (q/stroke-weight 3)
      (doseq [[a b] (:connected maze)]
        (q/line (to-point a) (to-point b))))))

(defn maze-canvas [queue]
    (q/sketch
    :title "Lost?"
    :size [400 400]
    :setup (fn []
             (q/frame-rate 60)
             (q/smooth)
             {:maze nil :queue queue})
    :update #'update-state
    :draw #'draw-state
    ;; :on-close #(async/close! queue)
    :features [:keep-on-top]
    :middleware [m/fun-mode]))


(defn create-maze
  "Constructor for creating an initial maze with the given size"
  [width height]
  {:width width
   :height height
   :visited #{}
   :connected #{}
   :active '()})

;; Implement the maze-generator algo using a step function that will return the 'next' version of the maze.
;; An alternative to a recursive function (or loop) that will make it easier to push new versions of the maze
;; to the drawing from a function that 'drives' the algorithm by stepping through the 'collection'.

(defn select-any
  "Inefficient randomizer fn - select one element"
  [col]
  (if (empty? col) nil
      (nth col (-> (new Random) (.nextInt (count col))))))

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
(defn not-visited-neighbours [{visited :visited h :height w :width} coord]
  (let [n (neighbours w h coord)
        pred (fn [c] (not (visited c)))]
    (filter pred n)))
(defn select-next-to-visit [maze coord] (select-any (not-visited-neighbours maze coord)))

(defn select-random-cell [width height]
  [(rand-int width) (rand-int height)])

(defprotocol MazeGenerator "A generator for mazes"
             (initiate-state [this s] "Adds necessary entries to the state")
             (step [this] "Generate the next state. Can be used as an 'iterate' function"))

(defrecord DepthFirst []
  MazeGenerator
  (initiate-state [this {w :width h :height :as s}]
    (-> this
        (merge s)
        (assoc :active '())
        (assoc :current (select-random-cell w h))))
  (step [this]
    (let [{done :done
           visited :visited
           connected :connected
           active :active
           current :current :as maze} this]
      (if done maze ;; Return if we're done
          ;; Find not visited-cells of the current cell. Select a random, and move
          ;; there (update current and active) If there are none, pop :active and assign
          ;; to :current. If there is no active left - we're done.
          (if-let [selected (select-next-to-visit maze current)]
            (-> (assoc maze :current selected)
                (assoc :connected (conj connected [current selected])) ;; Record a connection between this and next cell.
                (assoc :visited (conj visited current))
                (assoc :active (conj active current)))
            (if-let [selected (first active)]
              (-> (assoc maze :current selected)
                  (assoc :active (rest active))
                  (assoc :visited (conj visited current)))
              (assoc maze :done true)))))))

;; 1. Find an active.
;;     - if no active: done! 
;; 2. Set active as current
;; 3. Find a visited near current
;; 4. Connect them
;; 5. Remove current from active
;; 6. Add current to visited

(defn visited? [maze cell] (some #(= cell %) (:visited maze)))
(defn active? [maze cell] (some #(= cell %) (:active maze)))
(defn prims-frontier-candidates [{width :width height :height :as maze} reference]
  (let [not-visited-and-not-active (complement (fn [cell] (or (visited? maze cell) (active? maze cell))))]
    (filter not-visited-and-not-active (neighbours width height reference))))

(defrecord Prims []
  MazeGenerator
  (initiate-state [this {w :width h :height :as s}]
    (let [start-cell (select-random-cell w h)]
      (-> this
          (merge s)
          (assoc :current start-cell)
          (assoc :visited #{start-cell})
          (assoc :active (vec (prims-frontier-candidates s start-cell)))))) ;; :active is a vector
  (step [this]
    (let [{done :done
           visited :visited
           connected :connected
           active :active
           width :width
           height :height
           current :current} this]
      (if done this
          (if (empty? active) (assoc this :done true)
              (let [selected (rand-nth active)
                    visited-pred (fn [coord] (contains? visited coord))
                    selected-pred (fn [coord] (= coord selected))
                    neighbour (rand-nth (filter visited-pred (neighbours width height selected)))]
                (-> this
                    (assoc :connected (conj connected [selected neighbour]))
                    (assoc :visited (conj visited selected))
                    (assoc :active (remove selected-pred active))
                    ((fn [m] (let [frontier-cells (vec (prims-frontier-candidates m selected))]
                               (if-not (empty? frontier-cells)
                                 (assoc m :active
                                        (apply conj (:active m) frontier-cells))
                                 m))))
                    (assoc :current selected))))))))

;; TODO (above): Need to add new 'active' (frontier) cells

(defn run-maze
  "Runner function that will render a maze to a queue"
  [width height generator queue]
  (let [first-maze (initiate-state generator (create-maze width height))
        pred (fn [x] (not (:done x)))
        steps (take-while pred (iterate step first-maze))]
    (a/go
      (doseq [s steps] (a/>!! queue s)))))


;; == Usage: ==
;; (def q (a/chan 10))
;; (maze-canvas q)
;; (run-maze 20 20 q)
