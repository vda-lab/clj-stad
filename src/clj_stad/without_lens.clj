(ns clj-stad.core)

;;;;;;;;;;;; Arguments
; (def data-file "data/five_circles.csv")
; (def sample-size 100)
; (def nr-epochs 30)
; (def max-temp 1)
; (def min-temp 0.01)
; (def min-rf 0)
; (def max-rf 10000)
;;;;;;;;;;;;;

; (def raw-data (with-open [reader (io/reader data-file)]
;                 (doall
;                  (csv-data->maps (csv/read-csv reader)))))
; (def data
;   (->> raw-data
;        (shuffle)
;        (take sample-size)
;        (map #(assoc % :x (Integer/parseInt (:x %))))
;        (map #(assoc % :y (Integer/parseInt (:y %))))
;        (map #(assoc % :hue (Integer/parseInt (:hue %))))
;        (map #(assoc % :id (Integer/parseInt (:id %))))))
; (def values (map #(vector (:x %) (:y %)) data))
; (def hiD-dist-matrix (euclidean-distance-matrix values))

(defn full-graph [hiD-dist-matrix]
  (->> hiD-dist-matrix
       (get-all-edges) ; all-edges
       (apply lg/weighted-graph)))
(defn weighted-mst [hiD-dist-matrix]
  (la/prim-mst (full-graph hiD-dist-matrix)))

(defn mst [hiD-dist-matrix]
  (apply lg/graph (lg/edges (weighted-mst hiD-dist-matrix))))

(defn full-graph-unique-edges [hiD-dist-matrix]
  (->> (lg/edges (full-graph hiD-dist-matrix))
       (map sort)     ; ((0 7) (0 20) (0 27) (0 1) (0 24) ...
       (set)))

(defn mst-unique-edges [hiD-dist-matrix]
  (->> (lg/edges (mst hiD-dist-matrix))
       (map sort)     ; ((0 7) (0 20) (0 27) (0 1) (0 24) ...
       (set)))

(defn sorted-non-mst-edges-with-weight [hiD-dist-matrix]
  (->> (set/difference (full-graph-unique-edges hiD-dist-matrix) (mst-unique-edges hiD-dist-matrix)) ; non-mst-edges
       (map #(conj (vec %) (cm/mget hiD-dist-matrix (first %) (second %)))) ; non-mst-edges-with-weight
       (sort-by #(nth % 2))))

(defn temperature-seq [min-temp max-temp nr-epochs]
  (simanneal.anneal/make-temperature-seq max-temp min-temp nr-epochs))

(defn random-factor-seq [min-rf max-rf nr-epochs]
  (simanneal.anneal/make-random-factor-seq min-rf max-rf nr-epochs))

(defn result [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (simanneal.anneal/run-sa
          hiD-dist-matrix
          (sorted-non-mst-edges-with-weight hiD-dist-matrix) ; the edges to pick from
          (mst hiD-dist-matrix)
          (temperature-seq min-temp max-temp nr-epochs)
          (random-factor-seq min-rf max-rf nr-epochs)))

(defn optimal-nr-edges [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (second (result hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs)))

(defn optimal-graph [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (first (result hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs)))


(defn draw-xy-plot [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (let [r (result hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs)]
    (proto-repl-charts.charts/custom-chart
       "Evolution of objective function value"
       {:data {:x "x"
               :y "y"
               :columns [(flatten ["x" (nth r 2)])
                         (flatten ["y" (nth r 3)])]
               :type "scatter"}})))

(defn draw-x-plot [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (proto-repl-charts.charts/line-chart
    "history-x"
    {"x" (nth (result hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs) 2)}))

(defn draw-y-plot [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (proto-repl-charts.charts/line-chart
    "history-y"
    {"y" (nth (result hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs) 3)}))

(defn draw-optimal-graph [hiD-dist-matrix min-temp max-temp min-rf max-rf nr-epochs]
  (proto-repl-charts.graph/graph
    "sTAD network"
    (optimal-graph hiD-dist-matrix)))

; (draw-xy-plot hiD-dist-matrix)
; (draw-optimal-graph hiD-dist-matrix)
