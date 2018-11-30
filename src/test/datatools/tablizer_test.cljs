(ns datatools.tablizer-test
  (:require [cljs.test :refer (deftest is are)]
            [datatools.tablizer :as t]))

;;(deftest will-fail (is (= 1 2)))

(deftest test-collect-vec
  (let [res (t/collect (fn [x] [0 x]) [1 2 3])]
    (is (= [0 1 0 2 0 3] res))))


(deftest test-collect-list
  (let [res (t/collect (fn [x] '(x)) [1 2 3])]
    (is (= '(x x x) res))))
    

(deftest test-seq-like?
  (are [a b] (= a b)  
    true (t/seq-like? [])
    true (t/seq-like? ())
    false (t/seq-like? {})))


(deftest test-map-like?
      (are [a b] (= a b)  
        true (t/map-like? {})
        false (t/map-like? [])))


(deftest test-map-keys-one
  (let [expected  {"foo.:bar" 1} 
        actual (t/pfix-keys "foo" {:bar 1})]
    (is (= expected actual))))


;; Expect equivellent map placed but in vector.
(deftest test-spread-to-identity
  (let [expected [{:foo {:bar {:baz 42}}}] 
        actual (t/spread-to [:foo :bar :baz] 
          {:foo {:bar {:baz 42}}})]
    (is (= expected actual))))


(deftest test-spread-to-nil
  (let [expected [] 
        actual (t/spread-to [:foo :bar :baz] 
          {:foo {:bar {:zap 42}}})]
    (is (= expected actual))))


(deftest test-spread-to-one-level
  (let [expected [{:foo 42}] 
        actual (t/spread-to [:foo] {:foo 42})]
    (is (= expected actual))))


(deftest test-spread-to-one-level-vec1
  (let [expected [{:foo 42}] 
        actual (t/spread-to [:foo] {:foo [42]})]
    (is (= expected actual))))


(deftest test-spread-to-one-level-vec2
  (let [expected [{:foo 41} {:foo 42}] 
        actual (t/spread-to [:foo] {:foo [41 42]})]
    (is (= expected actual))))


(deftest test-spread-to-unexpected-type
  (let [expected [] 
        actual (t/spread-to [:foo :bar :baz] {:foo {:bar 42}})]
    (is (= expected actual))))


(deftest test-spread-to-a
  (let [expected '({:foo {:bar {:baz 41}}} 
                  {:foo {:bar {:baz 42}}}) 
        actual (t/spread-to [:foo :bar :baz] 
          {:foo {:bar 
            [{:baz 41} 
             {:baz 42}]}})]
    (is (= expected actual))))


(deftest test-spread-to-b
  (let [expected '({:foo {:bar {:baz 41}}
                   :zap {:lol "x"}} 
                  {:foo {:bar {:baz 42}}
                   :zap {:lol "x"}}) 
        actual (t/spread-to [:foo :bar :baz] 
          {:foo {:bar 
            [{:baz 41} 
             {:baz 42}]}
            :zap {:lol "x"}})]
    (is (= expected actual))))


(deftest test-collapse-maps-1
  (let [expected '({":foo" 42})
        actual (t/collapse-maps {:foo 42})]
    (is (= expected actual))))


(deftest test-collapse-maps-2
  (let [expected '({":foo.:bar" 42})
        actual (t/collapse-maps [{:foo {:bar 42}}])]
    (is (= expected actual))))


(deftest test-collapse-maps-2-2
  (let [expected '({":foo.:bar" 42 ":foo.:fiz" 1})
        actual (t/collapse-maps {:foo 
                                  {:bar 42 :fiz 1}})]
    (is (= expected actual))))


(deftest test-collapse-maps-vec
  (let [expected '({":foo.:bar.:baz" [{":zap" 42}]})
        actual (t/collapse-maps [{:foo 
                                  {:bar 
                                    {:baz 
                                      [{:zap 42}]}}}])]
    (is (= expected actual))))


(deftest test-nested-value 
  (let [expected 42 
        actual (t/nested-value [:foo :bar] {:foo {:bar 42}})] 
    (is (= expected actual))))


(def nvlu (t/nested-values-lookup 
  [:foo :bar] 
  [{:foo 
    {:bar 42}}
   {:foo 
    {:bar "x"}}]))


(deftest test-nested-values-lookup
  (let [expected '({:nested-value 42, :root {:foo {:bar 42}}})
        actual (nvlu 42)]
    (is (= expected actual))))


(deftest test-left-join-1
  (let [expected '({:left {:foo 1}, :rights ({:bar 1})})
        actual (t/left-join [:foo] [:bar] [{:foo 1}] [{:bar 1}])]
    (is (= expected actual))))


(deftest test-left-join-2
  (let [expected '({:left {:foo {:bar 1}}, :rights ({:fiz {:buz 1}})})
        actual (t/left-join 
                [:foo :bar] [:fiz :buz] 
                [{:foo {:bar 1}}] 
                [{:fiz {:buz 1}}])]
    (is (= expected actual))))


(def lj-2-1 (t/left-join 
                [:foo :bar] [:fiz] 
                [{:foo {:bar 1}}] 
                [{:fiz 1}]))

(deftest test-left-join-2-1
  (let [expected '({:left {:foo {:bar 1}}, :rights ({:fiz 1})})
        actual lj-2-1]
    (is (= expected actual))))


(deftest test-lmerge-collapse-left-join-2-1
  (let [expected '({":foo.:bar" 1 ":fiz" 1})
        actual (t/merge-collapse-left-join lj-2-1)]
    (is (= expected actual))))