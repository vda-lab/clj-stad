(ns clj-stad.core
  (:require [clojure.data.csv :as csv]
;            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.core.matrix :as cm]
            [incanter.core :as ico]
            [loom.graph :as lg]
            [loom.alg :as la]
            [clojure.math.numeric-tower :as math]
            [incanter.stats :as is]))
(use 'prc)

(def ^:dynamic *verbose* false)

(defmacro printfv
  [fmt & args]
  `(when *verbose*
     (printf ~fmt ~@args)))

(defmacro with-verbose
  [& body]
  `(binding [*verbose* true] ~@body))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
	  (rest csv-data)))

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn bin [n-bins xs]
  (let [min-x    (apply min xs)
        max-x    (apply max xs)
        range-x  (- max-x min-x)
        bin-fn   (fn [x]
                   (-> x
                       (- min-x)
                       (/ range-x)
                       (* n-bins)
                       (int)
                       (min (dec n-bins))))]
    (map bin-fn xs)))

(defn keyword2id [kw]
  (Integer/parseInt (clojure.string/replace (str kw) #":n" "")))

(defn id2keyword [i]
  (keyword (str "n" i)))


(defn write-file [list-of-lists filename]
  (with-open [out-data (io/writer filename)]
    (csv/write-csv out-data list-of-lists)))


;; **
;;; #### Euclidean distance calculation
;; **

(defn- sqr
  "Uses the numeric tower expt to square a number"
  [x]
  (math/expt x 2))

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (math/sqrt (euclidean-squared-distance a b)))

(defn euclidean-distance-row [a b]
  (vec (map #(round 3 (euclidean-distance b %)) a)))

(defn euclidean-distance-matrix [a]
  (vec (map #(euclidean-distance-row a %) a)))

;; **
;;; #### Cosine distance calculation
;; **

(defn cosine-similarity [a b]
  (let [dot-product (->> (map * a b)
                         (apply +))
        magnitude (fn [d]
                    (->> (map ico/sq d)
                         (apply +)
                         (ico/sqrt)))]
    (round 3 (/ dot-product (* (magnitude a) (magnitude b))))))
(defn cosine-distance [a b]
  (round 3 (- 1 (cosine-similarity a b))))

(defn cosine-distance-row [a b]
  (vec (map #(round 3 (cosine-distance b %)) a)))

(defn cosine-distance-matrix [a]
  (vec (map #(cosine-distance-row a %) a)))

(defn get-all-edges [dist-matrix]
  ; Same as hiD-dist-matrix, but as array of arrays, including the node IDs
  ; e.g ([0 1 0.134] [0 2 0.12])
  ; Should *NOT* be used for getting the distance between 2 points. Easier to do that with the graph afterwards, e.g.
  ; (lg/weight full-graph 0 7)
  (for [from (range (first (cm/shape dist-matrix)))
        to (range (+ from 1) (first (cm/shape dist-matrix)))]
    [from to (cm/mget dist-matrix from to)]))

;;;;; Create tulip output files

; (defn graph-to-csv-nodes [graph]
;   (->>
;    (lg/nodes graph)
;    (map #(conj [] % % (:x (nth data %)) (:y (nth data %)) (:hue (nth data %))))))
;
; (defn graph-to-csv-edges [graph]
;   (->> (la/prim-mst-edges graph)
; ;       (filter #(< (first %) (second %)))
;        (sort)))
;
; (defn create-tulip-csv [graph prefix]
;   (let [nodes-file (str "output/" prefix "_nodes.csv")
;         edges-file (str "output/" prefix "_edges.csv")]
;     (spit nodes-file "id,name,x,y,hue\n")
;     (with-open [out-data (io/writer nodes-file :append true)]
;       (csv/write-csv out-data (graph-to-csv-nodes graph)))
;     (spit edges-file "source,target\n")
;     (with-open [out-data (io/writer edges-file :append true)]
;       (csv/write-csv out-data (graph-to-csv-edges graph)))))

(defn graph-distance [graph from to]
  "Calculates the distance in steps between two nodes"
  (->> (la/shortest-path graph from to)
      (count)))

(defn graph-distance-row [graph node]
 (->> graph
      (lg/nodes)
      (sort)
      (map #(graph-distance graph node %))
      (vec)))

(defn graph-distance-matrix [graph]
  (->> graph
       (lg/nodes)
       (sort)
       (pmap #(graph-distance-row graph %))))

 (defn objective-function [hiD-dist-matrix graph-dist-matrix]
   (is/correlation
    (->> hiD-dist-matrix
 ;         (cm/slices)
 ;         (flatten)
 ;         (map vec)
         (flatten))
    (->> graph-dist-matrix
 ;         (cm/slices)
 ;         (flatten)
 ;         (map vec)
         (flatten))))

 (defn run-range [start stop step hiD-dist-matrix mst sorted-non-mst-edges-with-weight]
   (let [history-x (range start stop step)
         ;;;;; THERE IS AN ERROR HERE
         history-y (->> history-x
                        (map #(take % sorted-non-mst-edges-with-weight))
                        (map #(apply lg/add-edges mst %))
                        (pmap #(objective-function hiD-dist-matrix (graph-distance-matrix %))))]
     [history-x history-y]))

(defn normalise-matrix [m]
 (cm/div (cm/sub m (cm/emin m)) (cm/sub (cm/emax m) (cm/emin m))))

(defn add-multiple-edges [original-graph edges-to-add]
 (loop [new-graph original-graph
        edges edges-to-add]
   (if (= edges [])
     new-graph
     (recur (lg/add-edges new-graph (first edges)) (drop 1 edges)))))
