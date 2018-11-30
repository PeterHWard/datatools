(ns datatools.schema-test
  (:require [cljs.test :refer (deftest is are)]
            [datatools.schema :as s]
            [lib.helpers :as h]))


(defn mk-schemas 
  ([max-depth] (mk-schemas max-depth 1 []))
  ([max-depth depth prefix] 
    (if (not (> depth max-depth))
      (let [opath (conj prefix (str "o" depth))
            mpath (conj prefix (str "m" depth))
            nxt-depth (+ depth 1)]

      (flatten [{:path opath :cardinality :one} 
                (mk-schemas max-depth nxt-depth opath)
                {:path mpath :cardinality :many}
                (mk-schemas max-depth nxt-depth mpath)]))
    [])))


(defn mk-tree [depth] (s/schemas->tree (mk-schemas depth)))


(def tree-3 (mk-tree 3))


(defn just-paths [schemas] 
  (sort (map #(:path %) schemas)))
 

(deftest test-mk-schemas-1 
  (let [expected '({:path ["o1"], :cardinality :one} 
                   {:path ["m1"], :cardinality :many})
       actual (mk-schemas 1)]
    (is (= expected actual))))


(deftest test-mk-schemas-2 
  (let [expected '( {:path ["o1"], :cardinality :one} 
                    {:path ["o1" "o2"], :cardinality :one} 
                    {:path ["o1" "m2"], :cardinality :many} 
                    {:path ["m1"], :cardinality :many} 
                    {:path ["m1" "o2"], :cardinality :one} 
                    {:path ["m1" "m2"], :cardinality :many})
        actual (h/distinct-by #(:path %) (mk-schemas 2))]
    (is (= expected actual))))


(deftest test-mk-tree-1 
  (let [expected  { :key :datatools.schema/root
                    :path '() 
                    :cardinality :one
                    :children [{ :key "o1"
                                  :cardinality :one
                                  :path ["o1"]
                                  :children ()}
                                { :key "m1"
                                  :cardinality :many
                                  :path ["m1"]
                                  :children ()}]}
        actual (mk-tree 1)]
    (is (= expected actual))))


(deftest test-mk-tree-2 
  (let [expected { :key :datatools.schema/root, 
                   :path ()
                   :cardinality :one
                   :children '({ :path ["o1"], 
                                 :cardinality :one
                                 :key "o1"
                                 :children ({ :path ["o1" "o2"]
                                              :cardinality :one
                                              :key "o2"
                                              :children ()} 
                                            { :path ["o1" "m2"]
                                              :cardinality :many
                                              :key "m2",
                                              :children ()})} 

                                { :path ["m1"]
                                  :cardinality :many
                                  :key "m1"
                                  :children ({:path ["m1" "o2"]
                                              :cardinality :one, 
                                              :key "o2", 
                                              :children ()} 
                                            {:path ["m1" "m2"]
                                              :cardinality :many
                                              :key "m2"
                                              :children ()})})}

       actual (mk-tree 2)]
    (is (= expected actual))))


(deftest test-flat-paths-1 
  (let [expected '(["o1"])
        actual (just-paths (s/flat-paths (mk-tree 1)))]
    (is (= expected actual))))


(deftest test-flat-paths-2 
  (let [expected '(["o1"] ["o1" "o2"])
        actual (just-paths (s/flat-paths (mk-tree 2)))]
    (is (= expected actual))))


(deftest test-flat-subpaths-1a 
  (let [expected '(["o1"])
        actual (just-paths (s/flat-subpaths ["o1"] (mk-tree 1)))]
    (is (= expected actual))))


(deftest test-flat-subpaths-1b 
  (let [expected '(["m1"] ["o1"])
        actual (just-paths (s/flat-subpaths ["m1"] (mk-tree 1)))]
    (is (= expected actual))))


(deftest test-flat-subpaths-3m 
  (let [expected '(["m1"] ["o1"] 
                   ["m1" "m2"] ["m1" "o2"] ["o1" "o2"] 
                   ["m1" "m2" "m3"] ["m1" "m2" "o3"] 
                   ["m1" "o2" "o3"] ["o1" "o2" "o3"])
        actual (just-paths (s/flat-subpaths ["m1" "m2" "m3"] (mk-tree 3)))]
    (is (= expected actual))))


(deftest test-flat-subpaths-3o
  (let [expected '(["o1"] ["o1" "o2"] 
                   ["o1" "o2" "o3"])
        actual (just-paths (s/flat-subpaths ["o1" "o2" "o3"] (mk-tree 3)))]
    (is (= expected actual))))