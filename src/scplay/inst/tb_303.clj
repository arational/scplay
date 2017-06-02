(ns scplay.inst.tb-303
  (:use [overtone.core]))

(definst tb-303
  "A clone of the sound of a Roland TB-303 bass synthesizer."
  [note     {:default 30 :min 0 :max 255 :doc "midi note value input wave"}
   wave     {:default 0 :min 0 :max 1 :step 1 :doc "0=saw, 1=square"}
   cutoff   {:default 30 :min 0 :max 255 :doc "bottom rlpf frequency"}
   env      {:default 30 :min 0 :max 255 :doc "+ cutoff is top of rlpf frequency"}
   res      {:default 0.2 :min 0.1 :max 1.0 :doc "rlpf resonance"}
   sus      {:default 0.0 :min 0.0 :max 1.0 :doc "sustain level"}
   dec      {:default 1.0 :min 0.0 :max 1.0 :doc "decay"}
   amp      {:default 1.0 :min 0.0 :max 1.0 :doc "output amplitude"}
   gate     1         ; on/off control
   action   FREE
   position 0]
  (let [freq-val   (midicps note)
        amp-env    (env-gen (envelope [10e-10, 1, 1, 10e-10]
                                          [0.01, sus, dec]
                                          :exp)
                              :gate gate :action action)
        filter-env (env-gen (envelope [10e-10, 1, 10e-10]
                                          [0.01, dec]
                                          :exp)
                              :gate gate :action action)
        waves      [(* (saw freq-val) amp-env)
                    (* (pulse freq-val 0.5) amp-env)]
        tb303      (rlpf (select wave waves)
                         (+ (midicps cutoff) (* filter-env (midicps env))) res)]
    (* amp (pan2 tb303 position))))
