(ns scrap.example-metrics
  (:require [scrap.example-node :as node]
            [scrap.policy :as policy]))

(defn saturating-complexity-score
  [complexity]
  (let [{:keys [cap rise-rate floor]} policy/complexity]
    (if (<= complexity 1)
      floor
      (+ floor
         (* (- cap floor)
            (- 1.0
               (Math/exp (- (* rise-rate
                               (double (dec complexity)))))))))))

(defn api-contract-example?
  [metrics line-count phases]
  (and (<= (:assertions metrics) 2)
       (<= (+ (:branches metrics) (:table-branches metrics)) 4)
       (<= (:table-branches metrics) 1)
       (<= (:with-redefs metrics) 0)
       (<= (:helper-calls metrics) 1)
       (<= (:helper-hidden-lines metrics) 0)
       (<= (:temp-resources metrics) 0)
       (<= (:large-literals metrics) 0)
       (<= (count (:subject-symbols metrics)) 4)
       (<= phases 1)
       (<= line-count 18)))

(defn scored-setup-depth
  [metrics api-contract?]
  (if api-contract?
    (max 0 (- (:max-setup-depth metrics) 2))
    (:max-setup-depth metrics)))

(defn scored-branch-penalty
  [metrics table-driven? api-contract?]
  (let [branch-penalty (if table-driven?
                         (:table-branches metrics)
                         (+ (:branches metrics) (:table-branches metrics)))]
    (if api-contract?
      (max 0 (- branch-penalty 2))
      branch-penalty)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:48:01.780357-05:00", :module-hash "1095064886", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "740668023"} {:id "defn/saturating-complexity-score", :kind "defn", :line 5, :end-line 13, :hash "623672219"} {:id "defn/api-contract-example?", :kind "defn", :line 15, :end-line 27, :hash "-268962645"} {:id "defn/scored-setup-depth", :kind "defn", :line 29, :end-line 33, :hash "-217133315"} {:id "defn/scored-branch-penalty", :kind "defn", :line 35, :end-line 42, :hash "-356787910"}]}
;; clj-mutate-manifest-end
