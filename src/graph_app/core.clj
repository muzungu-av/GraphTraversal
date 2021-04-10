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

; I stopped writing this code!
; do not like how the storage of connections between the nodes of the graph (edges) is implemented.
; We all know there are 2 ways - adjacency matrix and adjacency list.
; The first has a drawback - a large memory consumption.
; In the second, a long search for edges, for example, directed graphs.
; I will try to implement my own way of representing a graph
; based on sorted lists without these disadvantages.
;
;
;Я перестал писать этот код!
;Не нравится, как реализовано хранение связей между узлами графа (ребрами).
;Все мы знаем, что есть 2 способа - матрица смежности и список смежности.
;У первого есть недостаток - большой расход памяти.
;Во втором - долгий поиск ребер, например ориентированных графов.
;Я попытаюсь реализовать свой собственный способ представления графа
;на основе отсортированных списков без этих недостатков.


(def M {:1 []
        :2 []
        :3 []})

(def G {
        :1 ['(:2 1) '(:6 2)]
        :2 ['(:3 4)]
        :3 ['(:4 1) '(:5 2) '(:8 2)]
        :4 []
        :5 []
        :6 ['(:7 7)]
        :7 ['(:8 9)]
        :8 ['(:9 1) '(:10 2)]
        :9 ['(:10 5)]
        :10 [],
        })

;;build-minimal-spanning-tree
(defn build-minimal-spanning-tree [G]
  (do
    (println "build-minimal-spanning-tree")
    G))

(defn ff [G s]
  (let [cnt (count (keys G))
        from (- cnt 1)
        to (* cnt (- cnt 1))]
    (if (and (>= s from) (<= s to))
      (do
        (build-minimal-spanning-tree G)
        (loop [j s]
              (if (<= j 0)
                G
                (match [G j]
                       [_ 1] (do (println from j) (recur (- j 1)))
                       [_ 2] (do (println from j) (recur (- j 1)))
                       [_ n] (do (println from n) (recur (- j 1)))))))
      (do (println "Wrong s!") G))))

