(ns advent-of-code.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private fuel-required
  (comp #(- % 2)
        #(Math/floor %)
        #(/ % 3)))

(defn- total-fuel-required [modules]
  (reduce + (for [module modules]
              (loop [acc 0
                     fuel (fuel-required module)]
                (if (<= fuel 0)
                  acc
                  (recur (+ acc fuel) (fuel-required fuel)))))))

(defn -main [& args]
  (let [[input-filepath output-filepath] args
        input-lines (line-seq (io/reader input-filepath))
        modules (map edn/read-string input-lines)]
    (spit output-filepath (int (total-fuel-required modules)))))

(comment
  (-main "input" "output")
  )
