(ns flurg.core
  (:use [overtone.live]
        [overtone.inst sampled-piano]
        [overtone.gui info control wavetable sequencer mixer stepinator])
  (:require [clojure.string :as s]))

(def upper-case-chars (map char (range 65 91)))
(def lower-case-chars (map char (range 97 123)))
(def alpha-chars (concat upper-case-chars lower-case-chars))
(def bang-lit \!)
(def period-lit \.)
(def question-lit \?)
(def lparen-lit \()
(def rparen-lit \))
(def all-chars (concat alpha-chars
                       [bang-lit
                        period-lit
                        question-lit
                        lparen-lit
                        rparen-lit]))

(def test-char \T)
(def test-word "The")
(def test-sentence "The house is brown.")

(def simple-degrees [:i :ii :iii :iv :v :vi :vii])

(def char-to-note-map
  (zipmap all-chars (cycle simple-degrees)))

(defn lookup-degree [c]
  (get char-to-note-map c))

(defn lookup-degrees [word scale-type root]
  (let [degree-seq (map lookup-degree (seq word))]
    (degrees->pitches degree-seq scale-type root)))

(defn play-word [word dur scale-type root]
  (doseq [note (lookup-degrees word scale-type root)]
    (sampled-piano note)
    (Thread/sleep dur)))

(defn sentence->word-seq [sentence]
  (s/split sentence #" "))

(defn play-sentence
  ([sentence]
     (play-sentence sentence 200 :major :c4))
  ([sentence dur scale-type root]
     (map #(play-word % dur scale-type root)
          (sentence->word-seq sentence))))

;; (defn play-chord [t note-seq]
;;   (map #(at (+ t (now)) (sampled-piano %)) note-seq))

;; (defn play-chord* [timed-chord-map]
;;   (let [t (:time timed-chord-map)
;;         notes (:notes timed-chord-map)]
;;     (play-chord t notes)))

;; (defn play-element [element t]
;;   (if (sequential? element)
;;     (doseq [note element]
;;       (at (+ t (now)) (sampled-piano note)))
;;     (at (+ t (now)) (sampled-piano note))))

;; (def times (take 220 (iterate #(+ 300 %) 0)))

;; (defn play-sequence [sequence]
;;   (doseq [t times]
;;     (doseq [e sequence]
;;       (at (+ t (now)) (play-element e t)))))

(comment
  (play-sentence "Some chinese guy." 200 :pentatonic :c#4)
  (play-sentence "But why?" 200 :minor :d4)
  (play-sentence "Because!" 200 :major :d4)
  (definst square-wave [freq 440] (square freq))
  [{:time 500, :notes (chord :c4 :major)}
   {:time 1000, :notes (chord :g4 :major)}
   {:time 1500, :notes (chord :d4 :major)}])