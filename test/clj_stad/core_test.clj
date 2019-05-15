(ns clj-stad.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clj-stad.core :refer :all]))

(defn get-raw-data [data-file]
  (with-open [reader (io/reader data-file)]
    (doall
       (csv-data->maps (csv/read-csv reader)))))

(defn get-data [raw-data]
    (->> raw-data
         (map #(assoc % :x (Integer/parseInt (:x %))))
         (map #(assoc % :y (Integer/parseInt (:y %))))
         (map #(assoc % :hue (Integer/parseInt (:hue %))))
         (map #(assoc % :id (Integer/parseInt (:id %))))))

(deftest test-dataloading
  (let [data-file "data/five_circles.csv"
        raw-data (get-raw-data data-file)
        data (get-data raw-data)]
    (testing "Are parameters defined?"
      (is (= "data/five_circles.csv" data-file)))
    (testing "Is data correctly loaded?"
      (is (= 348 (count raw-data)))
      (is (= 348 (count data)))
      (is (= "993" (:y (first raw-data))))
      (is (= "543" (:y (last raw-data)))))))

(deftest test-distance-matrix
  (let [data-file "data/five_circles.csv"
        raw-data (get-raw-data data-file)
        data (get-data raw-data)
        values (map #(vector (:x %) (:y %)) data)
        hiD-dist-matrix (euclidean-distance-matrix values)]
    (testing "Is hiD-distance-matrix correct?"
      (is (= [103 993] (first values)))
      (is (= 532.79 (second (first hiD-dist-matrix)))))))

(deftest test-intermediate-functions
  (let [data-file "data/five_circles.csv"
        raw-data (get-raw-data data-file)
        data (get-data raw-data)
        values (map #(vector (:x %) (:y %)) data)
        hiD-dist-matrix (euclidean-distance-matrix values)]
    (testing "Do deterministic functions work?"
      (is (= [95 195 14.56] (first (sorted-non-mst-edges-with-weight hiD-dist-matrix))))
      (is (= [316 329] (vec (first (mst-unique-edges hiD-dist-matrix))))))))
