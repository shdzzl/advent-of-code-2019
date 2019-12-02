(ns advent-of-code.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- total-fuel-required [modules]
  (->> modules
       (map #(/ % 3))
       (map #(Math/floor %))
       (map #(- % 2))
       (reduce +)))

(defn -main [& args]
  (let [[input-filepath output-filepath] args
        input-lines (line-seq (io/reader input-filepath))
        modules (map edn/read-string input-lines)]
    (spit output-filepath (int (total-fuel-required modules)))))

(comment
  (-main "input" "output")
  )
