(ns graph-app.core)

(defn traverse-graph-dfs [g s]
  (loop [vertices [] explored #{s} frontier [s]]   ;; вершины  исследованный граница
    (if (empty? frontier)
      vertices
      (let [v (peek frontier)
            neighbors (g v)]                                ;; соседи
        (recur
          (conj vertices v)
          (into explored neighbors)
          (into (pop frontier) (remove explored neighbors)))))))

(defn seq-graph-dfs [g s]
  ((fn rec-dfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-dfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} [s]))

(defn seq-graph-bfs [g s]
  ((fn rec-bfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-bfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj (clojure.lang.PersistentQueue/EMPTY) s)))