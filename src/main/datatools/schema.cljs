(ns datatools.schema 
  (:require [lib.helpers :as h]))

;; Schema example
;; {:path [:foo :bar]
;;  :cardinality :many}

(defn children-of [parent-path schemas] 
  (let [len (count parent-path)] 
    (filter (fn [schema] 
      (= parent-path (take len (:path schema)))) schemas)))
  


"""
  Example:
    { :key ::root 
      :path '() 
      :cardinality :one
      :children [{ :key :first 
                    :cardinality :one
                    :path [:first]
                    :children [...]}]}
"""
(defn schemas->tree-aux [schemas depth] 
  (map (fn [schema] (conj schema 
    { :key (last (:path schema))
      :children (schemas->tree-aux 
        (children-of (:path schema) schemas) (+ 1 depth))})) 
  
    (filter #(= depth (count (:path %))) schemas)))


(defn schemas->tree [schemas] 
  { :key ::root
    :path '()
    :cardinality :one
    :children (schemas->tree-aux 
      (h/distinct-by #(:path %) schemas) 1)})


(defn tree->schema [tree] (conj tree {:key nil :children nil}))


(defn flat-paths [tree] 
  (->> (:children tree) 
    (filter #(= :one (:cardinality %)) ,,,)
    (map (fn [c] [(flat-paths c) (tree->schema c)]) ,,,)
    flatten ,,,)) 


(defn flat-subpaths-aux [path tree]
  [(flat-paths tree)
            (->> (:children tree)
              (filter #(= (first path) (:key %)) ,,,)
              (map (fn [c] [(flat-subpaths-aux (rest path) c)
                            (tree->schema c)]))
              flatten)])


(defn flat-subpaths [path tree]
  (h/distinct-by #(:path %) 
    (flatten (flat-subpaths-aux path tree))))


