(ns datatools.tablizer)


;; Hard code for now
(def delimiter ".")


(defn collect [f coll] (flatten (map f coll)))


(defn type-str [any] (str (type any)))
(def vector-type-str (type-str []))
(def lazy-seq-str (type-str (lazy-seq)))
(def list-type-str (type-str '()))
(def map-type-str (type-str {}))


(defn seq-like? [coll]
  (if (or 
    (= (type coll) (type []))
    (= (type coll) (type (lazy-seq)))
    (= (type coll) (type ()))) true false))


(defn abs-type-string [any]
  (condp = (type-str any)
    vector-type-str "seq"
    lazy-seq-str "seq"
    list-type-str "seq"
    map-type-str "map"
    "unknown"))


(defn map-like? [coll]
    (if (= (type coll) (type {})) true false))


(defn always-seq [coll] 
  (if (seq-like? coll) coll [coll]))


(defn always-seq-map [f coll]
  (map f (always-seq coll)))


(defn pfix-keys-one [prefix coll]
  (reduce-kv (fn [m k v] (assoc m (str prefix delimiter k) v)) (empty coll) coll))


;; `coll` can be map or vector
(defn pfix-keys [prefix coll] 
  (let [bound #(pfix-keys-one prefix %)]
    (if (seq-like? coll)
      (bound coll)
      (bound coll))))


;; Produce single top-level seq with no intermediary seqs 
;; until parent of last entry in `path`. 
(defn spread-to [path root] 
  (let [objs (filter #(not= nil %) (always-seq root))]
    (if (seq path)
      (let [head (first path) 
            tail (rest path)]  
        (collect (fn [obj] 
          (map (fn [child] (assoc obj head child))
            (spread-to tail (head obj)))) objs))
        
    objs)))


(defn unspread [path root] )


;; expects map
(defn collapse-maps-one [root]
  (reduce-kv (fn [m k v] 
    (condp = (abs-type-string v)
      "seq" (assoc m (str k) (map collapse-maps-one v))
      
      "map" (reduce-kv (fn [m2 k2 v2] 
        (assoc m2 (str k delimiter k2) v2)) {} (collapse-maps-one v))
      
      (assoc m (str k) v))) {} root))


;; expects seq 
(defn collapse-maps [objs] 
  (map collapse-maps-one (always-seq objs)))
  

;; nil if any key maps to nil
(defn nested-value [path obj]
  (reduce (fn [m k] 
    (if (= nil m) m (k m))) obj path))


(defn nested-values-lookup [path coll]
;; memoize in map as successive lookups may require same value
  (let [nvs (map (fn [x] {:nested-value (nested-value path x) :root x}) coll)]
    (fn [datum] (filter #(= (:nested-value %) datum) nvs))))


;; Returns {:left map :rights seq<map>}
;; :rights can contain 0 or more 'map columns';
;; this means we always get left side even if 
;; nothing matches on right. 
(defn left-join [lpath rpath lobjs robjs]
  (let [lspread (spread-to lpath lobjs) 
        rspread (spread-to rpath robjs)
        rlookup (nested-values-lookup rpath rspread)] 
    (map (fn [lobj] (let [lval (nested-value lpath lobj)]
      {:left lobj :rights (map #(:root %) (rlookup lval))})) lspread)))


;; Placeholder until with deal with key collisions.
;; Where collisions primary value is retained. 
(defn merge-maps [primary secondary] (conj secondary primary))
 

(defn map-left-right [f lefts rights]
  (collect (fn [left] 
      (map (fn [right] 
        (f left right)) rights)) lefts))


;; Expects seq<{:left map :rights seq<map>}>
;; Original left map retained if :rights empty.
(defn merge-collapse-left-join [joins]
  (collect (fn [join] (let [lefts (collapse-maps (:left join)) 
                            rights (collapse-maps (:rights join))]
    (if (seq rights) 
      (map-left-right (fn [l r] (merge-maps l r)) lefts rights) 
      lefts))) joins))

  