(ns scplay.snake
  (:use [overtone.core]))


(defsynth snake [amp 1.0 freq 4000]
  (let [rand-dynamic (impulse 5)]
    (out 0
         (* amp
            (+ (+ 0.35 (* 0.35 (sin-osc (demand rand-dynamic 0 (dbrown 0.1 0.7 0.2 INF)))))
               (* 0.3
                  (sin-osc (demand rand-dynamic 0 (dbrown 5 20 3 INF)))))
            (pan2 (rhpf (gray-noise)
                        freq 0.1)
                  (* 0.7 (sin-osc (demand rand-dynamic 0 (dbrown 0.02 0.6 0.1 INF)))))))))

(snake 0.3 3000)
(snake 0.3 4000)
(snake 0.3 4500)
(snake 0.3 4700)
(snake 0.3 5000)
(kill snake)
