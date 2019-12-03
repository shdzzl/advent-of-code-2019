(ns advent-of-code.main
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def ^:private direction->transform
  {\U (fn [[x y] m] [x (+ y m)])
   \R (fn [[x y] m] [(+ x m) y])
   \D (fn [[x y] m] [x (- y m)])
   \L (fn [[x y] m] [(- x m) y])})

(defn- intersection-of-segments
  "Takes 2 segments and returns their intersection if there is one, nil
  otherwise."
  [[[x00 y00] [x01 y01]]
   [[x10 y10] [x11 y11]]]
  (let [s0x (- x01 x00)
        s0y (- y01 y00)
        s1x (- x11 x10)
        s1y (- y11 y10)
        sd (+ (* (* -1 s1x) s0y)
              (* s0x s1y))
        s (if (< 0 sd)
            (/ (+ (* (* -1 s0y) (- x00 x10))
                  (* s0x (- y00 y10)))
               sd)
            -1)
        td (+ (* (* -1 s1x) s0y)
              (* s0x s1y))
        t (if (< 0 td)
            (/ (- (* s1x (- y00 y10))
                  (* s1y (- x00 x10)))
               td)
            -1)]
    (when (and (>= s 0) (<= s 1)
               (>= t 0) (<= t 1))
      [(+ x00 (* t s0x))
       (+ y00 (* t s0y))])))

(defn- all-intersections-between-segments [segments0 segments1]
  (for [segment0 segments0
        segment1 segments1
        :let [intersection (intersection-of-segments segment0 segment1)]
        :when intersection]
    intersection))

(defn- decode-wire [wire-str]
  (->> (string/split wire-str #",")
       (map (fn [[direction & magnitude*]]
              [direction (edn/read-string (apply str magnitude*))]))))

(defn- wire-vectors->segments [[[d m] & vs]]
  (reduce (fn [acc [direction magnitude]]
            (let [[_ origin] (last acc)]
              (conj acc [origin ((direction->transform direction)
                                 origin magnitude)])))
          [[[0 0] ((direction->transform d) [0 0] m)]]
          vs))

(defn -main [& args]
  (let [[input-filepath output-filepath] args
        input-lines (line-seq (io/reader input-filepath))
        [wire1 wire2] (->> input-lines
                           (map decode-wire)
                           (map wire-vectors->segments))
        all-intersections (all-intersections-between-segments wire1 wire2)
        manhattan-distances (->> all-intersections
                                 (map (fn [[x y]]
                                        [(Math/abs (int x))
                                         (Math/abs (int y))]))
                                 (map #(reduce + %)))]
    (spit output-filepath (int (apply min manhattan-distances)))))

(comment
  (-main "input" "output")
  )
