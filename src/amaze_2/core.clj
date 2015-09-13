(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.async :as a :refer [<! >! <!! timeout chan alt! go]]))

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
(defn get-maze-cell
  "Get the value of a cell in a maze"
  [maze coord]
  (get-in maze [:cells coord]))
(defn get-fill-color [cell]
  (if (:current cell)
    [0 200 0]
    (if (:visited cell)
      [100 100 200]
      [255 255 255])))
(defn draw-state [{maze :maze}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 80)
  (let [to-rect #(scale-rect *cell-scale* (cell-coord-to-rect
                                  [(:width maze) (:height maze)]
                                  [(q/width) (q/height)] %))]
    (doseq [coord (keys (:cells maze))]
      (q/with-fill (get-fill-color (get-maze-cell maze coord))
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
  (let [maze {:width width :height height}
        coords (for [x (range 0 width), y (range 0 height)] [x y])]
    (assoc maze :cells (reduce #(assoc %1 %2 {}) {} coords))))

;; Sample usage:

;; (def q (a/chan 10))
;; (defn enqueue [maze queue]
;;   (a/>!! queue maze)
;;   maze)

;; (maze-canvas q)


;; (-> (create-maze 5 5)
;;     (enqueue q)
;;     (assoc-in [:cells [3 3]] {:visited true})
;;     (enqueue q)
;;     (assoc-in [:cells [3 2]] {:visited true})
;;     (enqueue q)
;;     (assoc-in [:cells [3 1]] {:current true})
;;     (enqueue q))
