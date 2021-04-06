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