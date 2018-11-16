(ns datatools.tablizer)

;; Schema example
;; {:path [:foo :bar]
;;  :cardinality :many}

"""(defn find-schema [schemas path] 
  (first 
    (filter (fn [s] (= (:path s) path) schemas))))"""

(defn collect [f coll] (flatten (map f coll)))

(defn seq-like? [coll]
  (if (= (type coll) (type {})) false true))


(defn map-like? [coll]
    (if (= (type coll) (type {})) true false))


(defn map-keys-one [prefix coll]
  (reduce-kv (fn [m k v] (assoc m (str prefix delimiter k) v)) (empty coll) coll))


;; `coll` can be map or vector
(defn map-keys [prefix coll] 
  (let [bound #(map-keys-one prefix %)]
    (if (seq-like? coll)
      (bound coll)
      (bound coll))))


;; Produce single top-level seq with no intermediary seqs 
;; until parent of last entry in `path`. 
(defn spread-to [root path] 
  (let objs (if (seq-like? root) root [root]) 
    (if (> 0 (count path))
      (let [head (first path) 
            tail (rest path)]  
        (map #(assoc root head %)
            (collect #(spread-to-aux (head %) tail) objs)))
        
  objs)))


;; Hard code for now
(def delimiter ".")


;; expects map
(defn collapse-maps-one [root]
  (reduce-kv (fn [m k v] 
    (if map-like v 
      (reduce-kv (fn [m2 k2 v2] 
        (assoc (collapse-maps-one m2) (str k delimiter k2) v2)) (empty v) v)  
      (assoc m k v))) (empty coll) coll))


;; expects seq 
(defn collapse-maps [objs] 
  (map collapse-maps-one objs))
  
;; nil if any key maps to nil
(defn nested-value [path obj]
  (reduce (fn [m k] 
    (if (= nil m) m (k m))) obj path))


(defn nested-values-lookup [path coll]
  (map #({:nested-value (nested-value path %) :root %}) coll))


;; Returns {:left map :rights seq<map>}
;; :rights can contain 0 or more 'map columns';
;; this means we always get left side even if 
;; nothing matches on right. 
(defn left-join [lpath rpath lobjs robjs]
  (let [lspread (spread-to lpath lobjs) rspread (spread-to rpath robjs)] 
    (let [rnvs (nested-values-lookup rspread)]
      (map #((let [lval (nested-value %)]
        {:left % rights: (filter #(= % lval) rnvs)})) lspread))))


;; Placeholder until with deal with key collisions.
;; Where collisions primary value is retained. 
(defn merge-maps [primary secondary] (assoc secondary primary))
 

;; Expects seq<{:left map :rights seq<map>}>
;; Original left map retained if :rights empty.
(defn merge-collapse-left-join [joins]
  (collect #(let [left (collapse-maps (:left %)) 
                  rights (collapse-maps (:rights %))]
    (if (> (count rights) 0) (map #(merge-maps left %) rights) [left])) joins))

  