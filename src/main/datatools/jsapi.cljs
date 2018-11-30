(ns datatools.jsapi 
  (:require [datatools.tablizer :as t]
            [datatools.schema :as s]))


(defn flatten-to [path root] 
  (cljs->js 
    (collapse-maps (t/spread-to (js->cljs path) (js->cljs root)))))


(defn left-join-flat [lpath rpath lobjs robjs] 
  (cljs->js 
    (t/merge-collapse-left-join (t/left-join
      (js->cljs lpath)
      (js->cljs rpath)
      (js->cljs lobjs)
      (js->cljs robjs)))))


(defn SchemaTree [schemas] 
  (let [tree (s/to-tree (js->cljs schemas))
    (reify 
      Object 
      (flat-subpaths [_ path]
        (s/flat-subpaths (js->cljs path) tree)))))