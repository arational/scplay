(ns scplay.ensemble
  (:use [overtone.core]))

(def port 44100)

(defonce server (osc-server port "osc-clj"))
;;(osc-listen server (fn [msg] (println msg)) :debug)
;;(osc-rm-listener server :debug)

;; TODO: use step
(defn param-osc-handler [{:keys [name min max step] :as param}
                         performance-atom]
  (fn [{:keys [args]}]
    (let [min (or min 0)
          max (or max 1.0)
          val (scale-range (first args) 0.0 1.0 min max)]
      (swap! performance-atom
             (fn [{:keys [stage mono-node] :as performance}]
               (let [param (keyword name)]
                 (when (and (not stage) mono-node)
                   (apply ctl mono-node param val))
                 (update performance
                         :params assoc param val)))))))

(defn mixer-osc-handler [instrument mixer-control]
  (let [control ({:vol inst-volume!
                  :pan inst-pan!} mixer-control)]
    (fn [{:keys [args]}]
      (control instrument (first args)))))

;; Usefull for combining phraseqs:
;; interleave
;; stretch

(defn- play-tone [node monophonic? metro phrase-beat params tone]
  (let [tone->params #(->> % :params
                           (merge params)
                           seq
                           flatten)
        beat (+ phrase-beat (:beat tone))
        node (at (metro beat)
                 (apply (if monophonic?
                          (partial ctl node)
                          node)
                        (tone->params tone)))
        length (:length tone)]
    (when length
      (let [beat (+ beat length)]
        (at (metro beat) (ctl node :gate 0))))
    node))

(defn perform [metro performance-atom]
  (swap! performance-atom
         (fn [{:keys [instrument stage params phrases-per-stage stage-phrases phrase-in-stage monophonic? mono-node] :as performance}]
           (if stage
             (let [phraseq (stage performance)
                   phrase (if phrases-per-stage
                            (stage-phrases phrase-in-stage)
                            (first phraseq))
                   phrase-beat (metro)
                   node (if mono-node
                          mono-node
                          instrument)
                   performance (if monophonic?
                                 (assoc performance :mono-node node)
                                 performance)
                   performance (if phrases-per-stage
                                 (assoc performance
                                        :phrase-in-stage (mod (inc phrase-in-stage)
                                                              phrases-per-stage))
                                 (assoc performance
                                        stage (rest phraseq)))]
               (apply-by (metro (+ phrase-beat (:length phrase)))
                         #'perform
                         [metro performance-atom])
               (doseq [tone (:tones phrase)]
                 (play-tone node
                            monophonic?
                            metro
                            phrase-beat
                            params
                            tone))
               performance)
             performance))))

(defn silence->phraseq [len]
  (repeat {:length len :tones []}))

(defn stretch-phraseq [n phraseq]
  (concat (repeat n (first phraseq))
          (lazy-seq (stretch-phraseq n (rest phraseq)))))

(defn delay-phraseq [len phraseq]
  (map (fn [{:keys [length tones] :as phrase}]
         (assoc phrase
                :length (+ length len)
                :tones (map (fn [tone]
                              (update tone :beat + len))
                            tones)))
       phraseq))

(defn combine-phraseqs [& phraseqs]
  (apply map
         (fn [& phrases]
           (let [lengths (mapv :length phrases)]
             {:length (apply + lengths)
              :tones (apply concat
                            (map (fn [tones delay]
                                   (map (fn [tone]
                                          (update tone :beat + delay))
                                        tones))
                                 (map :tones phrases)
                                 (cons 0 lengths)))}))
         phraseqs))

(defn merge-phraseqs [& phraseqs]
  (apply map
         (fn [& phrases]
           {:length (apply max (map :length phrases))
            :tones (apply concat (map :tones phrases))})
         phraseqs))

(defn silence->phraseq [len]
  (repeat {:length len :tones []}))

(defn rand-chord->phraseq [tone-len tone-delay chord-len chord-cnt chord]
  (let [len (* chord-cnt chord-len)
        rand-chord->phrase (fn [beat]
                             (mapv (fn [note delay]
                                     {:beat (+ beat delay)
                                      :length tone-len
                                      :params {:note note}})
                                   (apply rand-chord chord)
                                   (iterate (partial + tone-delay) 0)))]
    (->> (fn []
           {:length len
            :tones (apply concat (map rand-chord->phrase
                                      (range 0 len chord-len)))})
         repeatedly)))

(defn rand-rhythm->phraseq [len distances]
  (->> (fn []
         {:length len
          :tones ((fn phrase
                    ([]
                     (phrase 0))
                    ([beat]
                     (let [beat (+ beat (rand-nth distances))]
                       (when (< beat len)
                         (lazy-seq (cons {:beat beat}
                                         (phrase beat))))))))})
       repeatedly))

;; TODO: implement step
(defn handle-performance [path performance-atom]
  (let [{:keys [instrument params]} @performance-atom
        usable-param? (fn [param]
                        (contains? params (keyword (:name param))))
        handle-param (fn [{:keys [name] :as param}]
                       (prn (str path "/" name))
                       (osc-handle server (str path "/" name)
                                   (param-osc-handler param performance-atom)))
        params (->> instrument
                    :params
                    (filter usable-param?))]
    (doseq [param params] (handle-param param))
    nil))

(defn handle-instrument [instrument n]
  (let [unit-path (str "/mixer/" n)]
    (prn "registering osc-handler for mixer-unit with path:" unit-path)
    (osc-handle server
                (str unit-path "/vol")
                (mixer-osc-handler instrument :vol))
    (osc-handle server
                (str unit-path "/pan")
                (mixer-osc-handler instrument :pan))
    nil))

(defn handle-master []
  (osc-handle server
              "/mixer/master/vol"
              (fn [{:keys [args]}]
                (volume (first args)))))

(defn- seq-stage-phrases [performance]
  (let [{:keys [phrases-per-stage stage]} performance]
    (if phrases-per-stage
      (assoc performance
             :stage-phrases (->> (stage performance)
                                 (take phrases-per-stage)
                                 vec)
             stage (drop phrases-per-stage (stage performance)))
      performance)))

(defn- performer->effect-inst [{:keys [instrument effect params]}]
  (if effect
    (fn [& params]
      (inst-fx! instrument
                (fn [& args]
                  (apply effect (concat args params)))))
    instrument))

(defn- performance-ident->osc-path [ident]
  (str "/" (name ident)))

(defn begin [lineup metro]
  (let [performer->performance (fn [performer]
                                 (-> (if (:phrases-per-stage performer)
                                       (assoc performer
                                              :phrase-in-stage 0)
                                       performer)
                                     seq-stage-phrases
                                     (assoc :instrument
                                            (performer->effect-inst performer))
                                     atom))
        performances (map (fn [[ident performance]]
                            [ident (performer->performance performance)])
                          lineup)
        begin-performance (fn [performance-atom]
                            (let [{:keys [stage monophonic?]} @performance-atom]
                              (when (or (not stage) monophonic?)
                                (swap! performance-atom
                                       (fn [{:keys [instrument params]
                                             :as performance}]
                                         (assoc performance
                                                :mono-node (apply instrument
                                                                  (-> params
                                                                      seq
                                                                      flatten))))))
                              (when stage (perform metro performance-atom))))
        handle-stage-event (fn [{:keys [note]}]
                             (let [note-name (find-note-name note)]
                               (prn note-name)
                               (doseq [[_ performance-atom] performances]
                                 (swap! performance-atom
                                        (fn [performance]
                                          (if (contains? performance note-name)
                                            (-> (assoc performance
                                                       :stage note-name)
                                                seq-stage-phrases)
                                            performance))))))
        midi-event-key (java.util.UUID/randomUUID)]
    (doseq [[ident performance-atom] performances]
      (begin-performance performance-atom)
      (handle-performance (performance-ident->osc-path ident)
                          performance-atom))
    (on-event [:midi :note-on] handle-stage-event midi-event-key)
    {:performances (into {} performances)
     :stage-event-key midi-event-key}))

(defn end [ensemble]
  (remove-event-handler (:stage-event-key ensemble))
  (doseq [[ident performance-atom] (:performances ensemble)]
    (osc-rm-all-handlers server (performance-ident->osc-path ident))
    (swap! performance-atom
           (fn [performance]
             (-> performance
                 (dissoc :stage)
                 (update :mono-node
                         (fn [mono-node]
                           (when mono-node
                             (kill mono-node)
                             nil))))))))

(def mixer-buses {:master 0})

(defn target->bus [target]
  (cond (keyword? target) (target mixer-buses)
        (inst? target) (:bus target)
        :else (:master mixer-buses)))

(defn init-mixer [layout]
  (handle-master)
  (mapv (fn [[input-inst output-inst] n]
          (let [bus (if (keyword? output-inst)
                      (mixer-buses output-inst)
                      (:bus output-inst))]
            (ctl (:mixer input-inst) :out-bus bus)
            (handle-instrument input-inst n))
          input-inst)
        layout (range)))

(defn destroy-mixer [mixer]
  (osc-rm-all-handlers server "/mixer")
  (doseq [instrument mixer]
    (ctl (:mixer instrument) :out-bus (:master mixer-buses)))
  nil)
