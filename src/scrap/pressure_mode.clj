(ns scrap.pressure-mode
  (:require [scrap.pressure-score :as score]
            [scrap.pressure-stability :as stability]))

(defn- split-pressure?
  [summary]
  (let [avg-scrap (or (:avg-scrap summary) 0)
        harmful-duplication (or (:effective-duplication-score summary) 0)
        subject-repetition (or (:subject-repetition-score summary) 0)
        helper-hidden (or (:helper-hidden-example-count summary) 0)]
    (or (>= avg-scrap 10)
        (>= harmful-duplication 20)
        (>= subject-repetition 12)
        (pos? helper-hidden))))

(defn remediation-mode
  [summary blocks]
  (let [example-count (or (:example-count summary) 0)
        high-pressure-blocks (count (filter #(contains? #{"HIGH" "CRITICAL"} (score/pressure-level (:summary %))) blocks))]
    (cond
      (stability/stable-summary? summary) "STABLE"
      (and (not (stability/stable-summary? summary))
           (>= example-count 12)
           (or (>= high-pressure-blocks 2)
               (>= (or (:max-scrap summary) 0) 35))
           (split-pressure? summary))
      "SPLIT"
      :else "LOCAL")))
