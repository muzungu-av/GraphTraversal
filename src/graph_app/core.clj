(ns graph-app.core
  (:require [clojure.zip :as z])
  (:require [clojure.core.match :refer [match]]) )

(defn traverse-graph-dfs [g s]
  (loop [vertices [] explored #{s} frontier [s]]
    (if (empty? frontier)
      vertices
      (let [v (peek frontier)
            neighbors (g v)]
        (recur
          (conj vertices v)
          (into explored (map #(nth % 0) neighbors) )
          (into (pop frontier) (remove explored (map #(nth % 0) neighbors))))))))

(defn seq-graph-dfs [g s]
  ((fn rec-dfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-dfs
                     (into explored (map #(nth % 0) neighbors))
                     (into (pop frontier) (remove explored (map #(nth % 0) neighbors)))))))))
   #{s} [s]))

(defn seq-graph-bfs [g s]
  ((fn rec-bfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-bfs
                     (into explored (map #(nth % 0) neighbors))
                     (into (pop frontier) (remove explored (map #(nth % 0) neighbors)))))))))
   #{s} (conj (clojure.lang.PersistentQueue/EMPTY) s)))


(defn convert-int-str [v]
  "Convert a Integer number to a Upper case String in 36 base."
  (if (and (integer? v) (< v Integer/MAX_VALUE)) (clojure.string/upper-case (Integer/toString v 36)) nil))

(defn generate-series [n]
  "Generates a series of string values of length N."
  (let [series (take (+ n 1) (range))]
    (map convert-int-str series)))

(defn generate-empty-graph [n]
  "Generates a empty graph with size N"
  (let [res {}]
    (into res (map #(hash-map % []) (map #(keyword %) (generate-series n))))))

(defn find-key-in-depth [k coll]
  "Finding some value by the key in depth of collection."
  (let [coll-zip (z/zipper coll? seq nil coll)]
    (loop [x coll-zip]
      (when-not (z/end? x)
        (if-let [v (-> x z/node k)]
          v
          (recur (z/next x)))))))

(defn all-empty [g]
  "Return a seq of empty graph keys"
  (filter (fn [x](empty?(g x))) (keys g)))

(defn first-empty-pair [g]
  "Return a first & second empty graph keys"
  (let [a (all-empty g)]
    [(first a) (second a)]  ))

(defn link-pair-edges [G old-k new-k w]
  "Return a graph that contains old-k connected to new-k.
   It is not allowed duplicates.
   It can build a graph from an empty. (This is not a bug - this is a feature :)"
  (->>
    (loop [v  (old-k G)
           result [(list new-k w)]
           found #{new-k}]
      (if-let [[[k b] & tail] (seq v)]
        (if (contains? found k)
          (recur tail result found)
          (recur tail (conj result (list k b)) (conj found k)))
        result))
    (assoc G old-k)))

