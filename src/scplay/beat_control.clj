(ns scplay.beat-control
  (:use [overtone.core]))

(defonce root-trig-bus (control-bus))
(defonce root-cntr-bus (control-bus))
(defonce beat-trig-bus (control-bus))
(defonce beat-cntr-bus (control-bus))

(defsynth root-trig-synth [bpm 120]
  (out root-trig-bus (impulse bpm)))

(defsynth root-cntr-synth []
  (out root-cntr-bus (pulse-count (in:kr root-trig-bus))))

(defsynth beat-trig-synth []
  (out beat-trig-bus (pulse-divider (in:kr root-trig-bus) 60)))

(defsynth beat-cntr-synth []
  (out beat-cntr-bus (pulse-count (in:kr beat-trig-bus))))

(defonce root-trig (root-trig-synth))
(defonce root-cntr (root-cntr-synth))
(defonce beat-trig (beat-trig-synth))
(defonce beat-cntr (beat-cntr-synth))

(defonce bpm (atom 120))

(defn set-bpm [n]
  (reset! bpm n)
  (ctl root-trig :bpm n))
