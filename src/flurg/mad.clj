
(definst tone [note 60 amp 0.3 dur 0.4]
  (let [snd (sin-osc (midicps note))
        env (env-gen (perc 0.01 dur) :action FREE)]
    (* env snd amp)))

(defn p
  ([elements]
   (p elements (now)))
  ([[{:keys [synth vol pitch dur data]} & elements] t]
   (let [next-t (+ t (int (* 1000 dur)))]
     (at t
         (synth pitch vol dur))
     (when elements
       (apply-at next-t #'p elements [next-t])))))

(declare calc-duration)

(defn pattern
  ([m-element] (pattern m-element 1))
  ([m-element duration]
   (if (= (type []) (type m-element))
     (flatten
       (calc-duration m-element duration (count m-element)))
     (assoc m-element :dur (float duration)))))

(defn calc-duration
  [elements duration count]
  (map #(pattern % (/ duration count))
       elements))

(defn defnote
  [n-sym pitch]
  (intern *ns* n-sym
          {:synth tone
           :vol 0.2
           :pitch pitch
           :dur 0.1
           :data []}))

(defn def-notes
  "Define vars for all notes."
  []
  (doseq [octave (range 8)]
    (doseq [n (range 7)]
      (let [n-char (char (+ 65 n))
            n-sym (symbol (str n-char octave))
            note (octave-note octave (get NOTES (keyword (str n-char))))]
        (defnote n-sym note)
        (when-let [sharp (get NOTES (keyword (str n-char "#")))]
          (defnote (symbol (str n-char "#" octave))
                   (octave-note octave sharp)))
        (when-let [flat (get NOTES (keyword (str n-char "b")))]
          (defnote (symbol (str n-char "b" octave))
                   (octave-note octave flat)))))))

(def-notes)

(def derezzed [[E4 G4 E4] [E5 B4 G4 D4 A4 E4 G4 A4]])

(p (cycle (pattern (vec (take 8 (repeatedly (fn [] (vec (take 4 (repeatedly #(rand-nth [A4 B4 C4 D4 E4 F4 G4])))))))) 4)))

(stop)
(def derezzed [[] [E5 B4 G4 D4 A4 E4 G4 A4]])

(p (pattern derezzed 2))

(definst thx [gate 1]
  (let [target-pitches (map midi->hz [77 74 72 70 65 62 60 58 53 50 46 34 26 22 14 10])
        r-freq         (env-gen:kr (envelope [1 1 0.007 10] [8 4 2] [0 -4 1] 2) gate)
        amp-env        (env-gen:kr (envelope [0 0.07 0.21 0] [8 4 2] [0 1 1] 2) gate :action FREE)
        mk-noise       (fn [ug-osc]
                         (mix (map #(pan2 (ug-osc (+ (* r-freq (+ 230 (* 100 (lf-noise2:kr 1.3))))
                                                     (env-gen:kr (envelope [0 0 %] [8 6] [0 -3]))))
                                          (lf-noise2:kr 5))
                                   target-pitches)))
        saws           (mk-noise saw)
        sins           (mk-noise sin-osc)
        snd            (+ (* saws amp-env) (* sins amp-env))]
    (* 0.5 (g-verb snd 9 0.7 0))))

(thx)
(ctl thx :gate 0)