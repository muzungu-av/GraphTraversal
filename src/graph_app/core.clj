(ns graph-app.core
  (:require [clojure.string :as string]
            [clojure.zip :as z])
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
  ;;Convert a Integer number to a Upper case String in 36 base.
  (if (and (integer? v) (< v Integer/MAX_VALUE)) (string/upper-case (Integer/toString v 36)) nil))

(defn generate-series [n]
  ;;"Generates a series of string values of length N.
  (let [series (take (+ n 1) (range))]
    (map convert-int-str series)))

(defn generate-empty-graph [n]
  ;;"Generates a empty graph with size N
  (let [res {}]
    (into res (map #(hash-map % []) (map #(keyword %) (generate-series (dec n) ))))))

(defn find-key-in-depth [k coll]
  ;;"Finding some value by the key in depth of collection.
  (let [coll-zip (z/zipper coll? seq nil coll)]
    (loop [x coll-zip]
      (when-not (z/end? x)
        (if-let [v (-> x z/node k)]
          v
          (recur (z/next x)))))))

(defn all-empty [g]
  ;;Return a seq of empty graph keys
  (filter (fn [x](empty?(g x))) (keys g)))

(defn- some-weight [& [w]]
  (case w
    true (rand-int 100)
    0))

(defn link-pair-edges [G old-k new-k w]
  ;;Return a graph that contains old-k connected to new-k.
  ;;It is not allowed duplicates.
  ;;It can build a graph from an empty. (This is not a bug - this is a feature
  (let [w (some-weight w)]
    (->>
      (loop [v  (old-k G)
             result [(list new-k w)]
             found #{new-k}]
        (if-let [[[k b] & tail] (seq v)]
          (if (contains? found k)
            (recur tail result found)
            (recur tail (conj result (list k b)) (conj found k)))
          result))
      (assoc G old-k))))

(defn direct-prod
  ;;Produces the Cartesian product of one vertex of the graph and the others.
  ;;Uncomment if you need to get as [(), ...]
  [x y]
  ;(into []
    (for [a [x]
          b (remove #(= a %) y)]
      [a b]  ;`(~a ~b)
          )) ;)

(defn- my-reduce
  ;;A recursive function call with parameters with result accumulation.
  [rfn acc coll built_in max]
  (if-let [[x & xs] (seq coll)]
    (if-let [_ (<= (inc built_in) max)]
      (recur rfn (rfn acc x) xs (inc built_in) max)
      (vector acc built_in))
    (vector acc built_in)))

(defn- call-my-reduce [G peek built_in max weight]
  ;;Starts a recursion for the function.
  (let [prod (direct-prod peek (keys G))]
    (my-reduce
      (fn ([acc element] (link-pair-edges acc (first element) (second element) weight)))
      G prod built_in max)))

(defn build-graph [g s & [w]]
  ;;Builds a graph with vertexes (g) and edges (s).
  ;;The number of edges depends on the number of vertices. It can be from (N-1) to N(N-1).
  ;;The argument 'w' - is weight marker. If it is 'true' then a weight setting a random number else zero.
  (let [graph (generate-empty-graph g)
        all (all-empty graph)
        c   (count graph)
        min (- c 1)
        max (* c (- c 1))]
    (if (or (< s min ) (> s max))
      (println "The number of edges is out of bounds: " s)
      (loop [graph          graph
             peek           (first all)
             kinds          (dec (count all))
             built_in       0
             remaining      s
             check_alg      (>= kinds remaining)
             cycles         0]
        (if (< built_in remaining)
          (if check_alg
            (let [result (call-my-reduce graph peek built_in remaining w)
                  graph  (first result)]
              graph)
            (let [result    (call-my-reduce graph peek built_in remaining w)
                  graph     (first result)
                  built_in  (second result)
                  kinds     (count all)
                  check_alg (>= kinds remaining)
                  cycles    (inc cycles)]
              (recur graph (nth all cycles nil) kinds built_in remaining check_alg cycles)))
          graph)))))

(build-graph 4 5 true)
