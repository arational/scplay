(ns scplay.tb-303
  (:use [overtone.core]))

(defcgen tb-303
  "A clone of the sound of a Roland TB-303 bass synthesizer."
  [note   {:default 60 :doc "midi note value input"}
   wave   {:default 0 :doc "0=saw, 1=square"}
   cutoff {:default 100 :doc "bottom rlpf frequency"}
   env    {:default 1000 :doc "+ cutoff is top of rlpf frequency"}
   res    {:default 0.2 :doc "rlpf resonance"}
   dec    {:default 1.0 :doc "decay"}
   trig   {:doc "trigger signal ugen"}]
  (:ar
   (let [freq-val   (midicps note)
         amp-env    (env-gen (envelope [10e-10, 1, 1, 10e-10]
                                       [0.01, dec]
                                       :exp)
                             :gate trig)
         filter-env (env-gen (envelope [10e-10, 1, 10e-10]
                                       [0.01, dec]
                                       :exp)
                             :gate trig)
         waves      [(saw freq-val)
                     (pulse freq-val 0.5)]]
     (rlpf (select wave waves)
           (+ cutoff (* filter-env env)) res))))
