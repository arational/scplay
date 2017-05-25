(ns scplay.sphere
  (:use overtone.core))

(definst sphere [note {:default 60 :min 0 :max 255 :step 1}
                 vel {:default 0.7 :min 0.0 :max 1.0}
                 gate {:default 1 :min 0 :max 1 :step 1}
                 ffade {:default 8 :min 0 :max 16 :step 1}
                 attack {:default 4 :min 0 :max 10}
                 release {:default 4 :min 0 :max 10}
                 res {:default 0.2 :min 0.1 :max 1.0}]
  (let [freq (midicps note)
        output (* (/ 4)
                  (+ (saw freq)
                     (saw (+ freq 3))
                     (saw (+ freq 1/3))
                     (saw (- freq 1))))
        env (env-gen (adsr attack 1 1 release 1 0) :action FREE :gate gate)
        output (rlpf output (* ffade env freq) res)]
    (pan2 (* vel env output))))
