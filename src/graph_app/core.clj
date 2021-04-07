(ns graph-app.core)

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