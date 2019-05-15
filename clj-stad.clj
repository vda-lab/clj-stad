(ns stad
  (:require [clojure.java.io :as io]))
(load-file "./functions.clj")
(load-file "./simanneal.clj")

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
(def values (map #(vector (:x %) (:y %)) data))
(def hiD-dist-matrix
  (euclidean-distance-matrix values))
(def full-graph
  (->> hiD-dist-matrix
       (get-all-edges) ; all-edges
       (apply lg/weighted-graph)))
(def weighted-mst
  (la/prim-mst full-graph))

(def mst (apply lg/graph (lg/edges weighted-mst)))
(def full-graph-unique-edges
  (->> (lg/edges full-graph)
       (map sort)     ; ((0 7) (0 20) (0 27) (0 1) (0 24) ...
       (set)))

(def mst-unique-edges
  (->> (lg/edges mst)
       (map sort)     ; ((0 7) (0 20) (0 27) (0 1) (0 24) ...
       (set)))

(def sorted-non-mst-edges-with-weight
  (->> (set/difference full-graph-unique-edges mst-unique-edges) ; non-mst-edges
       (map #(conj (vec %) (cm/mget hiD-dist-matrix (first %) (second %)))) ; non-mst-edges-with-weight
       (sort-by #(nth % 2))))

(def nr-iterations 50)
(def temperature-seq (simanneal.anneal/make-temperature-seq 1 0.01 nr-iterations))
; (def temperature-seq (simanneal.anneal/make-temperature-seq 1.5 0.1 nr-iterations))
(def random-factor-seq (simanneal.anneal/make-random-factor-seq 0 10000 nr-iterations))


(def result
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
