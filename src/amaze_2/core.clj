(ns amaze-2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup [] [])

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


