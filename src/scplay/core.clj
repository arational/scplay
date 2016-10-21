(ns scplay.core
  (:use [overtone.core]
        [scplay.tb-303])
  (:require [scplay.beat-control :as bc]
            [scplay.marpix :as marpix]))

(def ctl-server (osc-server 44100 "osc-clj"))
(osc-listen ctl-server (fn [msg] (println msg)) :debug)
(osc-rm-listener ctl-server :debug)

(def arp-inst (marpix/marpix :buf marpix/buf))
(kill arp-inst)
(bc/set-bpm 100)
(ctl arp-inst :note 40)

(defn control-randomize [sub {:keys [args]}]
  (when (= 1.0 (first args))
    (marpix/randomize-sub sub)))

(defn control-mute [sub {:keys [args]}]
  (when (= 1.0 (first args))
    (marpix/mute-sub sub)))

(defn control-freq [{:keys [args]}]
  (let [note (scale-range (first args) 0 1 20 100)]
    (ctl arp-inst :note note)))

(def handle-xy
  (osc-handle ctl-server "/xymod"
              (fn [{:keys [args]}]
                (let [cutoff (scale-range (first args) 0 1 0 6000)
                      env (scale-range (second args) 0 1 1 10000)]
                  (ctl arp-inst
                       :cutoff cutoff
                       :env env)))))
(def handle-randomize
  [(osc-handle ctl-server "/randomize/1/1"
               (partial control-randomize 0))
   (osc-handle ctl-server "/randomize/1/2"
               (partial control-randomize 1))
   (osc-handle ctl-server "/randomize/1/3"
               (partial control-randomize 2))
   (osc-handle ctl-server "/randomize/1/4"
               (partial control-randomize 3))])
(def handle-mute
  [(osc-handle ctl-server "/mute/1/1"
               (partial control-mute 0))
   (osc-handle ctl-server "/mute/1/2"
               (partial control-mute 1))
   (osc-handle ctl-server "/mute/1/3"
               (partial control-mute 2))
   (osc-handle ctl-server "/mute/1/4"
               (partial control-mute 3))])
(def handle-freq
  (osc-handle ctl-server "/freq"
              control-freq))
