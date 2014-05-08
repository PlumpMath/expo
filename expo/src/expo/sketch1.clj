(ns expo.sketch1
  (:refer-clojure :exclude [fn])
  (:require [expo.quil-plus :refer :all]))

(defn setup []
  (smooth)                          ;; Turn on anti-aliasing
  (frame-rate 30)                    ;; Set framerate to 1 FPS
  (background 200))                 ;; Set the background colour to

(def counter (atom 0))
                      
(defn draw []
  (swap! counter inc)
  (run)) 

(defsketch example   
  :title "Sketch 1"  
  :setup setup                      
  :draw draw                        
  :size [323 200])                  

;; then interact via the repl!

(comment
  (do
    (push-fn (fn [] (background 0 0 255)))
    (push-fn (fn [] (let-mouse [x y] (ellipse x y 10 10))))
    (undo)
    (push-fn (fn [] (let-mouse [x y] (ellipse x y 20 20))))))

;; etc

;; use save-program when you're happy with the results
