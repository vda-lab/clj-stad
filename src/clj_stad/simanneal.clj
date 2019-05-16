(ns simanneal.anneal
  (:require [clojure.core.async :as async]
            [clojure.core.reducers :as r]
            [loom.graph :as lg]
            [incanter.stats :as is]))

(defn add-multiple-edges
  "Add edges to graph"
  [original-graph edges-to-add]
  (loop [new-graph original-graph
         edges edges-to-add]
    (if (= edges [])
      new-graph
      (recur (lg/add-edges new-graph (first edges)) (drop 1 edges)))))

(defn make-temperature-seq
  "produce an exponential cooling schedule from tmax to tmix"
  [tmax tmin steps]
  (let [tfactor (* -1 (Math/log (/ tmax tmin)))]
    (for [step (range steps)]
      (if (= step 0)
        tmax
        (* tmax (Math/exp (* tfactor (/ step (dec steps)))))))))

(defn make-random-factor-seq
  "produce a linear drop in jump distance"
  [rfmin rfmax steps]
  (reverse (map first (partition (int (/ (- rfmax rfmin) steps)) (range rfmin rfmax)))))

(defn nr-edges-to-add-remove-fn
  "Pick the number of edges to add or remove. Range to pick from is defined by random-factor"
  [random-factor]
  (let [jump (rand-int random-factor)]
    (if (> (rand) 0.5)
      jump
      (* -1 jump))))

(defn proposed-nr-edges-fn
  "Proposes number of edges.
   When <0 or > 10000: 'mirrors' back: divides by 2 and goes inward"
  [prev-nr-edges rf]
  (let [nr-edges-to-add-remove-fn (nr-edges-to-add-remove-fn rf)
        proposed-nr-edges (+ prev-nr-edges nr-edges-to-add-remove-fn)]
    (cond
      (< proposed-nr-edges 0)     (int (* -1 (/ proposed-nr-edges 2)))
      (> proposed-nr-edges 10000) (int (- 10000 (/ proposed-nr-edges 2)))
      :else proposed-nr-edges)))

(defn proposed-graph-fn
  "Creates new state"
  [initial-graph nr-edges list-of-all-edges]
  (add-multiple-edges initial-graph (take nr-edges list-of-all-edges)))

(defn stad-score [hiD-dist-matrix graph-dist-matrix]
  (is/correlation (flatten hiD-dist-matrix) (flatten graph-dist-matrix)))

(defn acceptance-probability
  [dE temp]
  (Math/exp (/ (* -1 dE) temp)))

(defn run-sa
  "Run simulated annealing.
  State is a GRAPH, _not_ a distance matrix.
  Repeatedly move-fn on the state, creating new states.
  Change to the proposed state if either one of two conditions occur
  - The proposed solution scores better
  - The proposed solution scores worse but within range
    defined by the delta, the temperature and some randomness"

  [hiD-dist-matrix
   sorted-non-mst-edges-with-weight ; the edges to pick from
   mst
   temperature-seq
   random-factor-seq]

  (let [state (atom mst)
        state-matrix (atom (clj-stad.core/graph-distance-matrix mst))
        state-score (atom (stad-score hiD-dist-matrix @state-matrix))
        nr-edges (atom 0)
        history-x (atom [])
        history-y (atom [])]
    (doseq [step (range 0 (count temperature-seq))]
      (let [temp (nth temperature-seq step)
            rf (nth random-factor-seq step)
            proposed-nr-edges (proposed-nr-edges-fn @nr-edges rf)
            proposed-graph (proposed-graph-fn mst proposed-nr-edges sorted-non-mst-edges-with-weight)
            proposed-graph-distance-matrix (clj-stad.core/graph-distance-matrix proposed-graph)
            proposed-score (stad-score hiD-dist-matrix proposed-graph-distance-matrix)
            dE (- @state-score proposed-score)]

        ; (println step "proposed score:" proposed-score)
        (swap! history-x conj proposed-nr-edges)
        (swap! history-y conj proposed-score)
        ; (if (> proposed-score @state-score)
        ;   (do
        ;     (println "Case A: better score:" @state-score proposed-score)
        ;     (reset! state proposed-graph)
        ;     (reset! state-matrix proposed-graph-distance-matrix)
        ;     (reset! state-score proposed-score)
        ;     (reset! nr-edges proposed-nr-edges))
        ;   (if (> (acceptance-probability dE temp) (rand))
        ;     (do
        ;       (println "Case B: up to chance:" dE temp (acceptance-probability dE temp))
        ;       (reset! state proposed-graph)
        ;       (reset! state-matrix proposed-graph-distance-matrix)
        ;       (reset! state-score proposed-score)
        ;       (reset! nr-edges proposed-nr-edges))))))


        (if (or
              (> proposed-score @state-score)
              (> (acceptance-probability dE temp) (rand)))
          (do
            ; (println proposed-nr-edges)
            (reset! state proposed-graph)
            (reset! state-matrix proposed-graph-distance-matrix)
            (reset! state-score proposed-score)
            (reset! nr-edges proposed-nr-edges)))))
    [@state @nr-edges @history-x @history-y]))
