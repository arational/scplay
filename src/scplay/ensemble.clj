(ns scplay.ensemble
  (:use [overtone.core]))

(def port 44100)

(defonce server (osc-server port "osc-clj"))
;;(osc-listen server (fn [msg] (println msg)) :debug)
;;(osc-rm-listener server :debug)

;; TODO: use step
(defn param-osc-handler [params-atom
                         control-param
                         mono-node]
  (let [{:keys [key min max step]} control-param]
    (fn [{:keys [args]}]
      (let [min (or min 0)
            max (or max 1.0)
            val (scale-range (first args) 0.0 1.0 min max)]
        (swap! params-atom
               (fn [params]
                 (when mono-node
                   (apply ctl mono-node key val))
                 (assoc params key val)))))))

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

(defn- buffered-staging? [staging]
  (boolean (:buffer-size staging)))

(defn- get-phrase [staging]
  (let [{:keys [stage
                stages
                buffer
                buffer-pos]} staging]
    (if (buffered-staging? staging)
      (buffer buffer-pos)
      (first (get stages stage)))))

(defn- seq-next-phrase [staging]
  (let [{:keys [stage
                buffer-size]} staging]
    (if (buffered-staging? staging)
      (update staging
              :buffer-pos
              #(mod (inc %) buffer-size))
      (update-in staging
                 [:stages stage]
                 rest))))

(defn perform [metro performance]
  (when-let [staging @(:staging performance)]
    (let [{:keys [instrument
                  monophonic?
                  mono-node]} performance
          params @(:params performance)
          phrase (get-phrase staging)
          phrase-beat (metro)
          node (if mono-node
                 mono-node
                 instrument)]
      (swap! (:staging performance) seq-next-phrase)
      (apply-by (metro (+ phrase-beat (:length phrase)))
                #'perform
                [metro performance])
      (doseq [tone (:tones phrase)]
        (play-tone node
                   monophonic?
                   metro
                   phrase-beat
                   params
                   tone))
      nil)))

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
(defn handle-performance [path control-params performance]
  (let [params-atom (:params performance)
        mono-node (:mono-node performance)]
    (doseq [control-param control-params]
      (let [key (:key control-param)]
        (prn (str path "/" (name key)))
        (osc-handle server
                    (str path "/" (name key))
                    (param-osc-handler params-atom
                                       control-param
                                       mono-node))))
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

(defn- seq-next-stage-to-buffer [staging]
  (if (buffered-staging? staging)
    (let [{:keys [buffer-size
                  buffer
                  stage
                  stages]} staging]
      (-> staging
          (assoc :buffer (->> (stage stages)
                              (take buffer-size)
                              vec))
          (update-in [:stages stage]
                     #(drop buffer-size %))))
    staging))

(defn- performer->effect-inst [{:keys [instrument effect params]}]
  (fn [& params]
    (inst-fx! instrument
              (fn [& args]
                (apply effect (concat args params))))))

(defn- performance-ident->osc-path [ident]
  (str "/" (name ident)))

(defn- performer->staging [performer]
  (let [{:keys [repeat-phrases
                stage
                stages]} performer
        staging {:stage stage
                 :stages stages}]
    (if repeat-phrases
      (assoc staging
             :buffer-size repeat-phrases
             :buffer-pos 0
             :buffer [])
      staging)))

(defn- performer->performance [performer]
  (let [{:keys [stage
                params
                instrument
                effect]} performer
        performance {:instrument (if effect
                                   (performer->effect-inst performer)
                                   instrument)
                     :params (atom params)}]
    (if stage
      (assoc performance
             :staging (-> performer
                          performer->staging
                          seq-next-stage-to-buffer
                          atom))
      performance)))

(defn- staged-performance? [performance]
  (boolean (:staging performance)))

(defn- begin-performance [metro performance]
  (let [monophonic? (:monophonic? performance)
        instrument (:instrument performance)
        params @(:params performance)
        performance (if (or (not (staged-performance? performance)) monophonic?)
                      (assoc performance
                             :mono-node (apply instrument
                                               (-> params
                                                   seq
                                                   flatten)))
                      performance)]
    (when (staged-performance? performance)
      (perform metro performance))
    performance))

(defn begin [lineup metro]
  (let [performances (->> lineup
                          (map (fn [[ident performance]]
                                 [ident (performer->performance performance)]))
                          (map (fn [[ident performance]]
                                 [ident (begin-performance metro performance)])))
        handle-stage-event (fn [{:keys [note]}]
                             (let [note-name (find-note-name note)]
                               (prn note-name)
                               (doseq [[_ performance] performances]
                                 (when-let [staging-atom (:staging performance)]
                                   (swap! staging-atom
                                          (fn [staging]
                                            (let [{:keys [stages]} staging]
                                              (if (contains? staging note-name)
                                                (-> staging
                                                    (assoc :stage note-name)
                                                    seq-next-stage-to-buffer)
                                                staging))))))))
        ;; begin all performances
        midi-event-key (java.util.UUID/randomUUID)]
    (doseq [[ident performance] performances]
      (when-let [control-params (get-in lineup [ident :control-params])]
        (handle-performance (performance-ident->osc-path ident)
                            control-params
                            performance)))
    (on-event [:midi :note-on] handle-stage-event midi-event-key)
    {:performances (into {} performances)
     :stage-event-key midi-event-key}))

(defn end [ensemble]
  ;; Remove the midi-event-handler
  (remove-event-handler (:stage-event-key ensemble))
  (let [performances (:performances ensemble)]
    ;; Remove all osc-handlers
    (doseq [[ident _] performances]
      (osc-rm-all-handlers server (performance-ident->osc-path ident)))
    ;; Stop the performances
    (doseq [[_ performance] performances]
      (when (staged-performance? performance)
        (reset! (:staging performance) nil))
      (when-let [mono-node (:mono-node performance)]
        (kill mono-node)))))

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
