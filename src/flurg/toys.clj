(ns flurg.core
  (:use [overtone.live]
        [clojure.core.match :only [match]]
        [overtone.inst sampled-piano]))


(comment
  (use 'overtone.core)
  (boot-external-server)
  (demo 5 (pan2 (sin-osc 440)))
  (definst beep [freq 440] (sin-osc freq))
  (beep)
  (stop)
  (beep)
  (ctl beep :freq 100)
  (stop)
  (demo 60
        (let [bpm 120
              notes [40 41 28 28 28 27 25 35 78]
              trig (impulse:kr (/ bpm 120))
              freq (midicps (lag (demand trig 0 (dxrand notes INF)) 0.25))
              swr (demand trig 0 (dseq [1 6 6 2 1 2 4 8 3 3] INF))
              sweep (lin-exp (lf-tri swr) -1 1 40 3000)
              wob (apply + (saw (* freq [0.99 1.01])))
              wob (lpf wob sweep)
              wob (* 0.8 (normalizer wob))
              wob (+ wob (bpf wob 1500 2))
              wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

              kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
              kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
              kick (clip2 kick 1)

              snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
              snare (+ snare (bpf (* 4 snare) 2000))
              snare (clip2 snare 1)]

          (clip2 (+ wob kick snare) 1)))

  (defsynth dubstep [bpm 120 wobble 1 note 50 snare-vol 1 kick-vol 1 v 1]
    (let [trig (impulse:kr (/ bpm 120))
          freq (midicps note)
          swr (demand trig 0 (dseq [wobble] INF))
          sweep (lin-exp (lf-tri swr) -1 1 40 3000)
          wob (apply + (saw (* freq [0.99 1.01])))
          wob (lpf wob sweep)
          wob (* 0.8 (normalizer wob))
          wob (+ wob (bpf wob 1500 2))
          wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

          kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
          kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
          kick (clip2 kick 1)

          snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
          snare (+ snare (bpf (* 4 snare) 2000))
          snare (clip2 snare 1)]

      (out 0    (* v (clip2 (+ wob (* kick-vol kick) (* snare-vol snare)) 1)))))

  (dubstep)
  (ctl 25 :wobble 4)
  (ctl 25 :note 35)
  (ctl 25 :bpm 250)
  (stop)
  )

(def phrase1a [:iii :v :iv# :iii :iii :ii# :iii :ii#])
(def phrase1b [:iii :v :iv# :iii :v# :vi :v# :vi])
(def phrase1c [:iii :v :iv# :iii :iii :ii# :i :vii- :vi- :vii- :vi- :vii- :i :vii- :vii- :vi-])

(def phrase2 [:i :ii :i :vii- :i :ii :i :vii- :i :vii- :vii- :vi-])

(def phrase3 [:iii :iv# :v# :vi :vii :ii#+ :vii :vi :vii :vi :vii :vi :vi :v# :iv :iii :iii :ii# :i :vii- :vii- :vi-])

(def phrase1a-reprise [:iii :v :iv# :iii :iii :ii#])
(def phrase1b-reprise [:iii :v :iv# :iii :v# :vi])

(def phrase1-bass [:vi--- [:vi- :iii- :i-] [:vi- :iii- :i-]])
(def phrase2-bass [:iii-- [:iii- :vii-- :v--] [:iii- :vii-- :v--]])

(def phrase3-bass [:ii--- [:vi-- :ii- :iv-] [:vi-- :ii- :iv-]])


(def right-hand-degrees (concat phrase1a phrase1b phrase1c
                                phrase1a phrase1b phrase1c
                                phrase2
                                phrase2
                                phrase3
                                phrase3
                                phrase2
                                phrase2
                                phrase1a-reprise
                                phrase1b-reprise
                                phrase1a-reprise
                                phrase1b-reprise
                                phrase2
                                phrase2
                                phrase3
                                phrase3
                                phrase2
                                phrase2))


(def left-hand-degrees (concat (apply concat (repeat 6 phrase1-bass))  ;;A
                               phrase2-bass                            ;;B
                               (apply concat (repeat 8 phrase1-bass))  ;;C
                               phrase2-bass                            ;;D
                               (apply concat (repeat 2 phrase1-bass))  ;;E
                               (apply concat (repeat 2 phrase3-bass))  ;;F
                               (apply concat (repeat 2 phrase1-bass))  ;;G
                               (apply concat (repeat 2 phrase3-bass))  ;;H
                               (apply concat (repeat 14 phrase1-bass)) ;;I
                               (apply concat (repeat 2 phrase3-bass))  ;;J
                               (apply concat (repeat 2 phrase1-bass))  ;;K
                               (apply concat (repeat 2 phrase3-bass))  ;;L
                               (apply concat (repeat 10 phrase1-bass)) ;;M
                               (apply concat (repeat 2 phrase3-bass))  ;;N
                               (apply concat (repeat 2 phrase1-bass))  ;;O
                               (apply concat (repeat 2 phrase3-bass))  ;;P
                               (apply concat (repeat 14 phrase1-bass)) ;;Q
                               (apply concat (repeat 2 phrase3-bass))  ;;R
                               (apply concat (repeat 2 phrase1-bass))  ;;S
                               (apply concat (repeat 2 phrase3-bass))  ;;T
                               phrase1-bass                            ;;U
                               ))

(def lh-pitches (degrees->pitches left-hand-degrees :major :Ab4))
(def rh-pitches (degrees->pitches right-hand-degrees :major :Ab4))

(def cur-pitch-rh (atom -1))
(def cur-pitch-lh (atom -1))

(defn reset-pos
  []
  (reset! cur-pitch-rh -1)
  (reset! cur-pitch-lh -1))

(defn vol-mul
  [vol]
  (* vol 0.002))

(defn play-next-rh
  [vol]
  (let [idx (swap! cur-pitch-rh inc)
        pitch (nth (cycle rh-pitches) idx)]
    (sampled-piano pitch (vol-mul vol))))

(defn play-next-lh
  [vol]
  (let [idx (swap! cur-pitch-lh inc)
        pitch (nth (cycle lh-pitches) idx)]
    (if (sequential? pitch)
      (doseq [p pitch]
        (sampled-piano p (vol-mul vol)))
      (sampled-piano pitch (vol-mul vol)))))

(defn play-both [n-pitches pause]
  (doseq [sound (interleave (take n-pitches (shuffle (cycle lh-pitches)))
                            (take n-pitches (cycle rh-pitches)))]
    (if (sequential? sound)
      (doseq [p sound]
        (let [idx (swap! cur-pitch-rh inc)
              pitch (nth (cycle sound) idx)]
          (sampled-piano pitch (vol-mul 100)))
        (Thread/sleep pause))
      (do (sampled-piano sound (vol-mul 100))
          (Thread/sleep pause)))))
(comment
  (play-both 10 500)


  (dotimes [n 100]
    (play-next-rh (rand-nth (range 70 100)))
    (Thread/sleep 100)
    (play-next-lh (rand-nth (range 70 100)))
    (Thread/sleep 100))

  (defn play-both [sleep-time vol]
    (dotimes [n 100]
      (play-next-rh (+ 80 (rand vol)))
      (Thread/sleep (+ 50 (rand sleep-time)))
      (play-next-lh (+ 80 (rand vol)))
      (Thread/sleep (+ 50 (rand sleep-time)))))
  (play-both 150 100)

  ;; (defonce m (poly/init "/dev/tty.usbserial-m64-0790"))

  ;; (poly/on-press m (fn [x y s]
  ;;                    (match [x y]
  ;;                           [7 _] (reset-pos)
  ;;                           [_ 0] (play-next-lh (+ (rand-int 5) (* 12 (+ x 4))))
  ;;                           [_ 7] (play-next-rh (+ (rand-int 5) (* 12 (+ x 4)))))))

  (let [idx (swap! cur-pitch-rh inc)
        pitch (nth (cycle rh-pitches) idx)]
    (repeatedly 100 (swap! lh-pitches conj pitch))
    (dotimes [n 0]
      (if (<= n 100)
        (do (play-next-lh 100)
            (Thread/sleep 500))
        (inc n))))

  (interleave #(repeatedly 100 (play-next-lh 100))
              #(repeatedly 100 (play-next-rh 100)))
  
  (def b (buffer 24 1))
  (buffer-write! b 0 (repeat 24 210))
  (demo 60
      (let [val (mouse-y 1000 200 1)
            pos (mouse-x 0 (- (buf-frames:kr b) 1))
            write (mouse-button)]
        (demand:kr write 0 (dbufwr val b pos 1))
        (* 0.1 (sin-osc (duty:kr (* 0.2 (dseq [0.5 0.75 0.5 1] INF)) 0 (dbufrd b (dseries 0 1 INF)))))))
  )