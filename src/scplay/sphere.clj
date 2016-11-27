(ns scplay.sphere
  (:use overtone.core))

(defsynth reverb
  "Implements Schroeder reverb using delays."
  [bus 0]
  (let [input (in bus)
        delrd (local-in 4)
        output (+ input [(first delrd) (second delrd)])
        sig [(+ (first output) (second output)) (- (first output) (second output))
             (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
             (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [44 143 188 210] 0.003) (control-dur))
        lout (local-out (delay-c sig deltimes deltimes))]
    (replace-out bus output)))

(definst sphere [note {:default 60 :min 0 :max 255 :step 1}
                 amp {:default 0.7 :min 0.0 :max 1.0}
                 gate {:default 1 :min 0 :max 1 :step 1}
                 ffade {:default 8 :min 0 :max 16}
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
    (pan2 (* amp env output))))
