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
      (swap! performance-atom update :params
             assoc (keyword name) val))))

(defn ctl-osc-handler [ctl performance-atom]
  (fn [{:keys [args]}]
    (let [val (first args)]
      (swap! performance-atom update ctl val))))

;; Usefull for combining phraseqs:
;; interleave
;; stretch

(defn- play-tone [instrument metro phase-beat params tone]
  (let [part->params #(->> % :params
                           (merge params)
                           seq
                           flatten)
        part (first tone)
        beat (+ phase-beat (:beat part))
        node (at (metro beat)
                 (apply instrument (part->params part)))]
    (loop [beat beat
           tone (next tone)]
      (when tone
        (let [part (first tone)
              beat (+ beat (:beat part))]
          (at (metro beat)
              (apply ctl node (part->params part)))
          (recur beat (next tone)))))))

(defn perform [metro performance-atom]
  (swap! performance-atom
         (fn [{:keys [instrument stop mute stage params phrases-per-stage stage-phrases phrase-in-stage] :as performance}]
           (when (not stop)
             (let [phraseq (stage performance)
                   phrase (if phrases-per-stage
                            (stage-phrases phrase-in-stage)
                            (first phraseq))
                   performance (if phrases-per-stage
                                 (assoc performance
                                        :phrase-in-stage (mod (inc phrase-in-stage)
                                                              phrases-per-stage))
                                 (assoc performance
                                        stage (rest phraseq)))
                   phrase-beat (metro)]
               (when (not mute)
                 (doseq [tone (:tones phrase)]
                   (play-tone instrument
                              metro
                              phrase-beat
                              params
                              tone)))
               (apply-by (metro (+ phrase-beat (:length phrase)))
                         #'perform
                         [metro performance-atom])
               performance))))
  nil)

(defn stretch-phraseq [n phraseq]
  (concat (repeat n (first phraseq))
          (lazy-seq (stretch-phraseq n (rest phraseq)))))

(defn delay-phraseq [len phraseq]
  (map (fn [{:keys [length tones] :as phrase}]
         (assoc phrase
                :length (+ length len)
                :tones (map (fn [tone]
                              (cons (update (first tone) :beat + len)
                                    (rest tone)))
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
                                          (cons (update (first tone)
                                                        :beat + delay)
                                                (rest tone)))
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
                                     [{:beat (+ beat delay)
                                       :params {:note note}}
                                      {:beat tone-len
                                       :params {:gate 0}}])
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
                         (lazy-seq (cons [{:beat beat}]
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
        handle-ctl (fn [ctl]
                     (osc-handle server (str path "/" (name ctl))
                                 (ctl-osc-handler ctl performance-atom)))
        params (->> instrument
                    :params
                    (filter usable-param?))]
    (doseq [param params] (handle-param param))
    (handle-ctl :mute)
    nil))

(defn- seq-stage-phrases [performance]
  (let [{:keys [phrases-per-stage stage]} performance]
    (if phrases-per-stage
      (assoc performance
             :stage-phrases (->> (stage performance)
                                 (take phrases-per-stage)
                                 vec)
             stage (drop phrases-per-stage (stage performance)))
      performance)))

(defn begin [lineup metro]
  (let [performer->performance (fn [performer]
                                 (-> (if (:phrases-per-stage performer)
                                       (assoc performer :phrase-in-stage 0)
                                       performer)
                                     seq-stage-phrases
                                     atom))
        performances (map (fn [[ident performance]]
                            [ident (performer->performance performance)])
                          lineup)
        handle-stage-event (fn [{:keys [note]}]
                             (let [note-name (find-note-name note)]
                               (prn note-name)
                               (doseq [[_ performance-atom] performances]
                                 (swap! performance-atom
                                        (fn [performance]
                                          (if (contains? performance note-name)
                                            (-> (assoc performance :stage note-name)
                                                seq-stage-phrases)
                                            performance))))))
        midi-event-key (java.util.UUID/randomUUID)]
    (on-event [:midi :note-on] handle-stage-event midi-event-key)
    (doseq [[ident performance-atom] performances]
      (handle-performance (str "/" (name ident)) performance-atom)
      (perform metro performance-atom))
    {:performances (into {} performances)
     :stage-event-key midi-event-key}))

(defn end [ensemble]
  (doseq [[_ performance-atom] (:performances ensemble)]
    (swap! performance-atom assoc :stop true))
  (remove-event-handler (:stage-event-key ensemble)))
