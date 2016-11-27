(ns scplay.core
  (:use [overtone.core]
        [scplay.tb-303]
        [scplay.sphere]
        [scplay.piano]
        [scplay.ensemble]))

(definst beep [note {:default 60}
               amp {:default 0.7 :min 0.0 :max 0.0 :step 0.1}]
  (* amp
     (* (env-gen (perc) :action FREE)
        (sin-osc (midicps note)))))

(definst far [amp 0.7]
  (let [input (* (env-gen (perc))
                 (square 440))
        delrd (local-in 2)
        output (+ input (delay-c (* delrd 0.6)
                                 0.1 0.1))]
    (local-out output)
    (* amp output)))

(def ensemble
  (begin {:sphere {:instrument sphere
                   :stage :single
                   :single (interleave (rand-chord->phraseq 3 1 4 1 [:C3 :minor 3 12])
                                       (rand-chord->phraseq 3 1 4 1 [:D3 :minor 3 12]))
                   :params {:amp 1.0}}
          :piano {:instrument piano
                  :stage :C3
                  :C3 (rand-chord->phraseq 1 1/4 4 1 [:C3 :minor 7 24])
                  :D3 (rand-chord->phraseq 1 1/4 4 1 [:D3 :minor 7 24])
                  :params {:vel 60
                           :release 0.8
                           :sustain 0.1}}
          :beep {:instrument beep
                 :stage :single
                 :single (rand-rhythm->phraseq 4 [1/4 1/2 1])
                 :params {:amp 0.6
                          :note 60}}}
        (metronome 60)))
;; (end ensemble)
;; (stop)
