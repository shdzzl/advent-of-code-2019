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
  (vary-meta coll merge {:should-halt? true}))

(def ^:private op-table
  {1 (vary-meta addition-op merge {:cursor-inc 4})
   2 (vary-meta multiplication-op merge {:cursor-inc 4})
   99 halt-op})

(defn- perform-op-at-cursor [cursor coll]
  (let [opcode (nth coll cursor)
        op (op-table opcode)]
    (vary-meta (op cursor coll) merge
               (select-keys (meta op)
                            [:cursor-inc]))))

(defn- run-program [coll]
  (loop [cursor 0
         program coll]
    (let [result (perform-op-at-cursor cursor program)
          {:keys [cursor-inc should-halt?]} (meta result)]
      (if should-halt?
        result
        (recur (+ cursor-inc cursor) result)))))

(defn find-noun-verb-pairs-producing [target program]
  (for [noun (range 0 100)
        verb (range 0 100)
        :let [this-program (assoc program 1 noun 2 verb)
              result (run-program this-program)]
        :when (= target (nth result 0))]
    [noun verb]))

(defn -main [& args]
  (let [[input-filepath output-filepath] args
        [input-program-str] (line-seq (io/reader input-filepath))
        input-program (mapv edn/read-string
                            (string/split input-program-str #","))
        [noun verb] (first (find-noun-verb-pairs-producing 19690720 input-program))]
    (spit output-filepath (int (+ (* 100 noun) verb)))))

(comment
  (-main "input" "output")
  )
