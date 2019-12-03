(ns advent-of-code.main
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn- addition-op [cursor coll]
  (let [[arg1 arg2 arg3] (map #(nth coll (+ % cursor)) [1 2 3])]
    (assoc coll arg3
           (+ (nth coll arg1)
              (nth coll arg2)))))

(defn- multiplication-op [cursor coll]
  (let [[arg1 arg2 arg3] (map #(nth coll (+ % cursor)) [1 2 3])]
    (assoc coll arg3
           (* (nth coll arg1)
              (nth coll arg2)))))

(defn- halt-op [cursor coll]
  (with-meta coll {:should-halt? true}))

(def ^:private op-table
  {1 addition-op
   2 multiplication-op
   99 halt-op})

(defn- perform-op-at-cursor [cursor coll]
  (let [opcode (nth coll cursor)
        op (op-table opcode)]
    (op cursor coll)))

(defn- run-program [coll]
  (loop [cursor 0
         program coll]
    (let [result (perform-op-at-cursor cursor program)]
      (if (:should-halt? (meta result))
        result
        (recur (+ cursor 4) result)))))

(defn -main [& args]
  (let [[input-filepath output-filepath] args
        [input-program-str] (line-seq (io/reader input-filepath))
        input-program (mapv edn/read-string
                            (string/split input-program-str #","))
        program-1202 (assoc input-program
                            1 12
                            2 2)
        result-program (run-program program-1202)]
    (spit output-filepath (int (nth result-program 0)))))

(comment
  (-main "input" "output")
  )
