(ns graph-app.core-test
  (:require [clojure.test :refer :all]
            [graph-app.core :refer :all]))

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

(deftest traverse-test
  (testing "Test traverse-graph-dfs function with weight graph."
    (is (= (traverse-graph-dfs G :1) [:1 :6 :7 :8 :10 :9 :2 :3 :5 :4]))))


(deftest dfs-test
  (testing "Test seq-graph-dfs function with weight graph."
    (is (= (seq-graph-dfs G :1)  [:1 :6 :7 :8 :10 :9 :2 :3 :5 :4]))))


(deftest bfs-test
  (testing "Test seq-graph-bfs function with weight graph."
    (is (= (seq-graph-bfs G :1)  [:1 :2 :6 :3 :7 :4 :5 :8 :9 :10]))))

(deftest convert-int-str-test
  (testing "Test convert-int-str function"
    (is (= (convert-int-str 2147483646) "ZIK0ZI"))
    (is (= (convert-int-str 0) "0"))
    (is (= (convert-int-str 360) "A0"))
    ))

(deftest generate-series-test
  (testing "Test generate-series function."
    (is (= (generate-series 5)  ["0" "1" "2" "3" "4" "5"]))))

(def empt-G {:0 [], :1 [], :2 [], :3 [], :4 [], :5 [], :6 [], :7 []})
(deftest generate-empty-graph-test
  (testing "Test generate-empty-graph function."
    (is (= (generate-empty-graph 7) empt-G))))

(deftest find-key-in-depth-test
  (testing "Test find-key-in-depth function."
    (is (= (find-key-in-depth :3 G) ['(:4 1) '(:5 2) '(:8 2)] ))))

(deftest find-key-in-depth-test
  (testing "Test link-pair-edges function."
    (let [X {:1 ['(:2 1) '(:3 2)]
             :2 ['(:3 4)]
             :3 []}]
      (is (= (link-pair-edges X :3 :1 "new")
             {:1 ['(:2 1) '(:3 2)], :2 ['(:3 4)], :3 ['(:1 "new")]})))))