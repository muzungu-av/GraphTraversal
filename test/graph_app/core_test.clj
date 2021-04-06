(ns graph-app.core-test
  (:require [clojure.test :refer :all]
            [graph-app.core :refer :all]))

(def G {:1 ['(:2 1) '(:3 2)]
        :2 ['(:4 4)]
        :3 ['(:4 2)]
        :4 [] })

(deftest traverse-test
  (testing "Test traverse-graph-dfs function with weight graph."
    (is (= (traverse-graph-dfs G :1) [:1 :3 :4 :2]))))


(deftest dfs-test
  (testing "Test seq-graph-dfs function with weight graph."
    (is (= (seq-graph-dfs G :1)  [:1 :3 :4 :2]))))


(deftest bfs-test
  (testing "Test seq-graph-bfs function with weight graph."
    (is (= (seq-graph-bfs G :1)  [:1 :2 :3 :4]))))

(deftest convert-int-str-test
  (testing "Test convert-int-str function"
    (is (= (convert-int-str 2147483646) "ZIK0ZI"))
    (is (= (convert-int-str 0) "0"))
    (is (= (convert-int-str 360) "A0"))
    ))