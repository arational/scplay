(ns scplay.inst.beep
  (:use overtone.core))

(definst beep [note {:default 60}
               vel {:default 0.7 :min 0.0 :max 0.0 :step 0.1}]
  (* vel
     (* (env-gen (perc) :action FREE)
        (sin-osc (midicps note)))))
