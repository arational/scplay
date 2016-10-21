(ns scplay.marpix
  (:use [overtone.core]
        [scplay.tb-303])
  (:require [scplay.beat-control :as bc]))

(defonce buf (buffer 16))

(definst marpix [note 30
                 wave 0
                 cutoff 100
                 env 1000
                 res 0.2
                 dec 1.0
                 amp 1.0
                 buf 0]
  (let [tick (pulse-divider:kr (in:kr bc/root-trig-bus) 15)
        cntr (pulse-count:kr tick)
        index (mod cntr 16)
        trig (and (buf-rd:kr 1 buf index)
                  tick)]
    (* amp (tb-303 note wave cutoff env res dec trig))))

(defn set-buf-sub! [sub f]
  (map (fn [n]
         (buffer-set! buf (+ sub (* n 4)) (f)))
       (range 4)))

(defn mute-sub [sub]
  (set-buf-sub! sub (constantly 0.0)))

(defn randomize-sub [sub]
  (set-buf-sub! sub #(choose [0.0 1.0])))
