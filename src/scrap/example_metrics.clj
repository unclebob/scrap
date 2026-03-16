(ns scrap.example-metrics
  (:require [scrap.example-node :as node]
            [scrap.shared :as shared]))

(defn saturating-complexity-score
  [complexity]
  (if (<= complexity 1)
    shared/trivial-complexity-floor
    (+ shared/trivial-complexity-floor
       (* (- shared/complexity-cap shared/trivial-complexity-floor)
          (- 1.0
             (Math/exp (- (* shared/complexity-rise-rate
                             (double (dec complexity))))))))))

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
