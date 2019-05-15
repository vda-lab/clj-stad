; (ns stad
;   (:require [clojure.data.csv :as csv]
;             [clojure.tools.logging :as log]
;             [clojure.java.io :as io]
;             [clojure.set :as set]
;             [clojure.core.matrix :as cm]
; ;            [gorilla-plot.core :as gc]
;             [incanter.core :as ico]
; ;            [incanter.charts :as ich]
;             [loom.graph :as lg]
;             [loom.alg :as la]
; ;            [loom.io :as li]
;             [clojure.math.numeric-tower :as math]
; ;              [ubergraph.core :as uc]
; ;              [ubergraph.alg :as ua]
; ;            [kixi.stats.core :as kc]
;             [incanter.stats :as is]))
; (use 'prc)

(ns stad
  (:require [clojure.java.io :as io]))
(load-file "./functions.clj")
(load-file "./simanneal.clj")

;; **
;;; # 1. Read data
;;;
;;; Result will be:
;;; data:
;;; ({:id 0, :x 103, :y 993, :hex "1F9F88", :hue 169}
;;;  {:id 1, :x 399, :y 550, :hex "27808E", :hue 188}
;;;  {:id 2, :x 388, :y 570, :hex "297B8E", :hue 191}
;;;  {:id 3, :x 375, :y 587, :hex "2B758E", :hue 195}
;;;  {:id 4, :x 367, :y 602, :hex "27AD81", :hue 160})
;; **

(def raw-data (with-open [reader (io/reader "data/five_circles.csv")]
                (doall
                 (csv-data->maps (csv/read-csv reader)))))
(def data
  (->> raw-data
       (shuffle)
       (take 174)
       (map #(assoc % :x (Integer/parseInt (:x %))))
       (map #(assoc % :y (Integer/parseInt (:y %))))
       (map #(assoc % :hue (Integer/parseInt (:hue %))))
       (map #(assoc % :id (Integer/parseInt (:id %))))))

; (clojure.pprint/pprint (take 5 data))
;
; ;;; Five circles
; (proto-repl-charts.charts/custom-chart
;  "Original five circles"
;  {:data {:x "x"
;          :columns [(flatten ["x" (map :x data)])
;                    (flatten ["y" (map :y data)])]
;          :color {:pattern (map #(str "#" (:hex %)) data)}
; ;         :color (map #(str "#" %) (map :hex data))
;          :type "scatter"}})

;; **
;;; # 2. Calculate hiD distance matrix
;;;
;;; Result will be:
;;; * distance matrix, called hiD-dist-matrix
;;; [[0.0    532.79 510.05]
;;;  [488.69 0.0    454.87]
;;;  [430.17 405.76 0.0   ]]
;; **
(def values (map #(vector (:x %) (:y %)) data))
(def hiD-dist-matrix
  (euclidean-distance-matrix values))

; (cm/shape hiD-dist-matrix)
; (cm/emin hiD-dist-matrix)
; (cm/emax hiD-dist-matrix)
;
;
; (def sample-size 348)
; (def sample-prob 0.01)


;; **
;;; # 3. Create full graph
;;;
;;; Result will be:
;;; * fully-connected weighted graph
;;; * mst
;; **
; (def all-edges (get-all-edges hiD-dist-matrix))
; (take 5 all-edges)
; (def full-graph (apply lg/weighted-graph all-edges))
; (def weighted-mst (la/prim-mst full-graph))

(def full-graph
  (->> hiD-dist-matrix
       (get-all-edges) ; all-edges
       (apply lg/weighted-graph)))
(def weighted-mst
  (la/prim-mst full-graph))

(def mst (apply lg/graph (lg/edges weighted-mst)))
; (proto-repl-charts.graph/graph
;    "MST"
;    mst)
;
;
;
; (defn normalise-matrix [m]
;   (cm/div (cm/sub m (cm/emin m)) (cm/sub (cm/emax m) (cm/emin m))))

;; **
;;; # 3. Create complete graph
;;;
;;; Here we'll create the complete graph.
;; **

;; **
;;; Helper methods
;; **


;; **
;;; Analysis
;; **
; (def all-edges (get-all-edges hiD-dist-matrix))
; (take 5 all-edges)
;
; (time (def full-graph
;         (apply lg/weighted-graph all-edges)))

;; **
;;; # 4. Create the MST
;;;
;;; Create the MST based on the full-graph. CAUTION: as this is an undirected graph, the edges might be present twice => need to use `mst-edges` instead of `(lg/edges mst)`
;; **

; (def weighted-mst (la/prim-mst full-graph))
;
;;; This does the same thing
;(def mst (apply lg/weighted-graph (la/prim-mst-edges full-graph)))

; (def mst (apply lg/graph (lg/edges weighted-mst)))

; (create-tulip-csv mst "bla")

; (proto-repl-charts.graph/graph
;  "MST"
;  mst)

;; **
;;; # 5. Create ordered list from which to pick the next edge(s)
;;;
;;; `non-mst` is an ordered list of edges (ordered based on distance from large to small) from which we'll pick the next edge to add.
;; **

(def full-graph-unique-edges
  (->> (lg/edges full-graph)
       (map sort)     ; ((0 7) (0 20) (0 27) (0 1) (0 24) ...
       (set)))

(def mst-unique-edges
  (->> (lg/edges mst)
;       (map #(select-keys % [:src :dest]))
;       (map vals)     ; ((0 7) (0 20) (0 27) (0 1) ... (7 0) (20 0) ...
       (map sort)     ; ((0 7) (0 20) (0 27) (0 1) (0 24) ...
       (set)))

; ((0 7) (0 20) (0 27) ...)
; (def non-mst-edges
;   (set/difference full-graph-unique-edges mst-unique-edges))
; (def non-mst-edges-with-weight
;   (map #(conj (vec %) (cm/mget hiD-dist-matrix (first %) (second %))) non-mst-edges))
; (def sorted-non-mst-edges-with-weight
;   (sort-by #(nth % 2) non-mst-edges-with-weight))

(def sorted-non-mst-edges-with-weight
  (->> (set/difference full-graph-unique-edges mst-unique-edges) ; non-mst-edges
       (map #(conj (vec %) (cm/mget hiD-dist-matrix (first %) (second %)))) ; non-mst-edges-with-weight
       (sort-by #(nth % 2))))

;;; # 6. Calculate graph-dist-matrix
;; **
;
; (time (def graph-dist-matrix (graph-distance-matrix mst)))
;
; (time (cm/pm (last (graph-distance-matrix mst))))

;;; ## 6b. Same thing based on http://www.loganlinn.com/blog/2013/04/22/dijkstras-algorithm-in-clojure/
;; **
; (def ^:private inf Double/POSITIVE_INFINITY)
;
; (defn update-costs
;   "Returns costs updated with any shorter paths found to curr's unvisisted
;   neighbors by using curr's shortest path"
;   [g costs unvisited curr]
;   (let [curr-cost (get costs curr)]
;     (reduce-kv
;      (fn [c nbr nbr-cost]
;        (if (unvisited nbr)
;          (update-in c [nbr] min (+ curr-cost nbr-cost))
;          c))
;      costs
;      (get g curr))))
;
; (defn dijkstra
;   "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
;   Graph is a map of nodes to map of neighboring nodes and associated cost.
;   Optionally, specify destination node to return once cost is known"
;   ([g src]
;    (dijkstra g src nil))
;   ([g src dst]
;    (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
;           curr src
;           unvisited (disj (apply hash-set (keys g)) src)]
;      (cond
;        (= curr dst)
;        (select-keys costs [dst])
;
;        (or (empty? unvisited) (= inf (get costs curr)))
;        costs
;
;        :else
;        (let [next-costs (update-costs g costs unvisited curr)
;              next-node (apply min-key next-costs unvisited)]
;          (recur next-costs next-node (disj unvisited next-node)))))))
;
; (use 'clojure.set)
; (defn convert-to-dijkstra [edges]
;   (apply merge-with union (map #(assoc {} (first %) {(last %) 1}) edges)))
;
; ;(def a (convert-to-dijkstra (lg/edges mst)))
; ;(def b (map #(dijkstra a %) (sort (lg/nodes mst))))
;
; (defn graph-in-dijkstra-format [g]
;   (convert-to-dijkstra (lg/edges g)))
;
; (defn dijkstra-distances-for-node [g n]
;   (sort-by first (dijkstra (graph-in-dijkstra-format g) n)))
; ;  (->> g
; ;       lg/nodes
; ;       sort
; ;       (map #(dijkstra (graph-in-dijkstra-format g) %))
; ;       (sort-by first)))
;
; (defn graph-distance-matrix [g]
;   (map #(dijkstra-distances-for-node g %) (sort (lg/nodes g))))

;(defn graph-distance-matrix [graph]
;  (->> graph
;       lg/nodes
;       sort
;       (map #(get-distances-for-row graph %))))

;; **
;;; ## 7. Compare hiD-dist-matrix to graph-dist-matrix
;; **

;(count graph-dist-matrix)

; (count (range 0 1200 50))
; ;
; (def complete-plot (run-range 0 1200 40 hiD-dist-matrix mst sorted-non-mst-edges-with-weight))
; (def history-x (first complete-plot))
; (def history-y (last complete-plot))
; (println history-x)
; (time (println history-y))
; (proto-repl-charts.charts/custom-chart
;  "Evolution of objective function value"
;  {:data {:x "x"
;          :columns [(flatten ["x" history-x])
;                    (flatten ["y" history-y])]
;          :type "scatter"}})
;
; (def new-graph
;   (->> sorted-non-mst-edges-with-weight
;        (take 1000)
;        (add-multiple-edges mst)))
;
; (proto-repl-charts.graph/graph
;   "New graph"
;   new-graph)

;;;;;;;;;;;;;;;;;
;; Run simulated annealing
;;;;;;;;;;;;;;;;;
(def nr-iterations 50)
(def temperature-seq (simanneal.anneal/make-temperature-seq 1 0.01 nr-iterations))
; (def temperature-seq (simanneal.anneal/make-temperature-seq 1.5 0.1 nr-iterations))
(def random-factor-seq (simanneal.anneal/make-random-factor-seq 0 10000 nr-iterations))


(def result
; (with-verbose
  (simanneal.anneal/run-sa
          hiD-dist-matrix
          sorted-non-mst-edges-with-weight ; the edges to pick from
          mst
          temperature-seq
          random-factor-seq))

(proto-repl-charts.charts/custom-chart
   "Evolution of objective function value"
   {:data {:x "x"
           :columns [(flatten ["x" (nth result 2)])
                     (flatten ["y" (nth result 3)])]
           :type "scatter"}})

(proto-repl-charts.charts/line-chart
  "history-x"
  {"x" (nth result 2)})

(proto-repl-charts.charts/line-chart
  "history-y"
  {"y" (nth result 3)})

(def new-graph
  (->> sorted-non-mst-edges-with-weight
       (take (second result))
       (add-multiple-edges mst)))

(proto-repl-charts.graph/graph
  "New graph"
  new-graph)
