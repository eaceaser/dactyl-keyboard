(ns dactyl-keyboard.hand
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]))

(defn palm [c]
  (let [palm-dimensions (get c :configuration-palm-dimensions)]
    (apply cube palm-dimensions)))

(defn finger [c segment-lengths segment-angles]
  (assert (= (count segment-lengths) (count segment-angles)))
  (let [finger-radius (get c :configuration-finger-radius)
        coord-fn (fn [trig] (map (fn [theta len] (* len (trig (* -1 theta)))) segment-angles segment-lengths))
        segment-x-positions (coord-fn (fn [theta] (Math/cos theta)))
        segment-y-positions (coord-fn (fn [theta] (Math/sin theta)))
        x-positions (cons 0 (reductions + segment-x-positions))
        y-positions (cons 0 (reductions + segment-y-positions))
        num-segments (count segment-lengths)]
    (map
      (fn [i length angle x-position y-position]
        (cond->> (cylinder finger-radius length :center false)
                 (= true) (rotate angle [1 0 0])
                 (> i 0) (translate [0 y-position 0])
                 (> i 0) (translate [0 0 x-position])))
      (range num-segments) segment-lengths segment-angles x-positions y-positions)
    )
  )

(defn fingers [c]
  (let [digit-matrix (get c :configuration-finger-lengths)
        digit-angles (get c :configuration-finger-angles)
        palm-dimensions (get c :configuration-palm-dimensions)
        palm-width (first palm-dimensions)
        finger-offset (/ palm-width (count digit-matrix))]
    (map
      (fn [i finger-lengths finger-angles]
        (cond->> (finger c finger-lengths finger-angles)
                 (> i 0) (translate [(* i finger-offset) 0 0])))
      (range (count digit-matrix)) digit-matrix digit-angles)
    ))

(defn model-hand [c]
  (let [palm-dimensions (get c :configuration-palm-dimensions)
        finger-radius (get c :configuration-finger-radius)
        hand-angle (get c :configuration-hand-angle)
        [palm-width palm-length palm-height] palm-dimensions
        fingers-x-offset (+ (* -1 (/ palm-width 2)) finger-radius)
        fingers-y-offset (/ palm-length 2)]
    (->> (union
           (palm c)
           (->> (fingers c)
                (rotate (/ pi 2) [1 0 0])
                (mirror [0 1 0])
                (translate [fingers-x-offset fingers-y-offset 0])))
         (rotate hand-angle [0 1 0]))))

(defn hand-width [c]
  (first (get c :configuration-palm-dimensions)))

(defn hand-length [c]
  (let [hand-length (second (get c :configuration-palm-dimensions))
        digit-angles (get c :configuration-finger-angles)
        digit-lengths (get c :configuration-finger-lengths)
        digit-x-positions (map (fn [angles lengths] (map (fn [theta len] (* len (Math/cos (* -1 theta)))) angles lengths)) digit-angles digit-lengths)
        total-digit-lengths (map (partial reduce +) digit-x-positions)
        max-finger-length (apply max total-digit-lengths)]
    (+ hand-length max-finger-length)))

(def hand-c {:configuration-palm-dimensions [84.78 94.02 29.66]
        :configuration-hand-angle      (deg2rad 20)
        :configuration-finger-radius   (/ 18.82 2)
        :configuration-thumb-lengths   [39.97 30.52]
        :configuration-finger-lengths  [[28.49 23.66 23.61]   ; index
                                        [31.98 30.82 26.33]   ; middle
                                        [29.08 27.30 25.04]   ; ring
                                        [19.88 23.34 20.31]]  ; pinky
        :configuration-finger-angles   [[0 (deg2rad 40) (deg2rad 53)] ; index
                                        [0 (deg2rad 53) (deg2rad 69)] ; middle
                                        [(deg2rad 29) (deg2rad 43) (deg2rad 50)] ; ring
                                        [(deg2rad 26) (deg2rad 35) (deg2rad 42)]]}) ; pinky

;(spit "things/hand.scad"
;      (write-scad (model-hand hand-c)))
;
;(defn -main [dum] 1)