(ns scrap.pressure-score
  (:require [scrap.pressure-stability :as stability]))

(defn- size-factor
  [summary]
  (let [example-count (or (:example-count summary) 0)]
    (cond
      (<= example-count 1) 0.25
      (<= example-count 2) 0.40
      (<= example-count 4) 0.65
      :else 1.0)))

(defn- score-terms
  [summary example-count]
  [(* 1.2 (or (:avg-scrap summary) 0))
   (* 0.6 (or (:max-scrap summary) 0))
   (* 0.8 (or (:effective-duplication-score summary) 0))
   (* 20 (stability/ratio (or (:low-assertion-examples summary) 0) example-count))
   (* 15 (stability/ratio (or (:branching-examples summary) 0) example-count))
   (* 15 (stability/ratio (or (:with-redefs-examples summary) 0) example-count))
   (* 12 (stability/ratio (or (:helper-hidden-example-count summary) 0) example-count))])

(defn refactor-pressure-score
  [summary]
  (let [example-count (or (:example-count summary) 0)
        base (reduce + (score-terms summary example-count))
        matrix-credit (* 1.5 (or (:case-matrix-repetition summary) 0))]
    (max 0 (- (* (size-factor summary) base) matrix-credit))))

(defn pressure-level
  [summary]
  (let [score (refactor-pressure-score summary)]
    (cond
      (stability/stable-summary? summary) "STABLE"
      (>= score 55) "CRITICAL"
      (>= score 35) "HIGH"
      (>= score 18) "MEDIUM"
      :else "LOW")))
