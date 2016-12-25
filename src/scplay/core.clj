(ns scplay.core
  (:use [overtone.core]
        [scplay.tb-303]
        [scplay.sphere]
        [scplay.piano]
        [scplay.ensemble]))
;; interesting isntruments:
;; prophet
;; tb303
;; simple-flute
;; bubbles

(def mixer
  (init-mixer [[sphere :master]
               [piano effectdummy]
               [beep effectdummy]
               [effectdummy :master]]))
(destroy-mixer mixer)

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

(definst effectdummy []
  (* (env-gen (perc 0.01 0.01) :action FREE)
     (saw 440)))


(def ensemble
  (begin {:sphere {:instrument sphere
                   :repeat-phrases 2
                   :stage :C4
                   :stages {:C4 (interleave (rand-chord->phraseq 3 0 4 1 [:C3 :minor 3 24])
                                            (rand-chord->phraseq 3 0 4 1 [:D3 :minor 3 24]))}
                   :params {:amp 0.8}}
          :piano {:instrument piano
                  :repeat-phrases 2
                  :stage :C3
                  :stages {:C3 (merge-phraseqs (rand-chord->phraseq 1/4 1/4 3/4 2 [:C3 :minor 3 24])
                                               (silence->phraseq 2))
                           :D3 (rand-chord->phraseq 1 1/4 4 1 [:D3 :minor 7 24])}
                  :control-params [{:key :vel :min 0 :max 100 :step 1}]
                  :params {:vel 60
                           :release 0.8
                           :sustain 0.1}}
          :beep {:instrument beep
                 :stage :single
                 :stages {:single (rand-rhythm->phraseq 4 [1/4 1/2 1])}
                 :params {:amp 0.6
                          :note 60}}
          :mono-test {:instrument effectdummy
                      :monophonic? true
                      :effect fx-feedback
                      :params {:decay 0.7}}}
        (metronome 60)))
(end ensemble)
(stop)
