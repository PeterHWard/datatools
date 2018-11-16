(ns datatools.test
  (:require [cljs.test :refer (deftest is)]))


(deftest test-collect
    (is (= [1 2 3] (collect [1 [2 3]])))


(deftest test-seq-like?
  (are [x y] (= x y)  
    true (seq-like? [])
    false (seq-like? {})))


(deftest test-map-like?
      (are [x y] (= x y)  
        true (map-like? {})
        false (map-like? [])))


(deftest test-map-keys-one
  (is (= {"foo.bar"} (map-keys "foo" {:bar}))))


;; Expect equivellent map placed but in vector.
(deftest test-spread-to-identity
  (is (= 
    [{:foo {:bar: :baz 42}}] 
    (spread-to {:foo {:bar: :baz 42}}))))

    (deftest test-spread-to-identity
      (is (= 
        [{:foo {:bar: :baz 42}}] 
        (spread-to {:foo {:bar: :baz 42}}))))