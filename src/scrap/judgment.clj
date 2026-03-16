(ns scrap.judgment
  (:require [scrap.pressure :as pressure]))

(defn summary-ratios
  [summary]
  (let [example-count (or (:example-count summary) 0)]
    {:low-assertion-ratio (pressure/ratio (or (:low-assertion-examples summary) 0) example-count)
     :zero-assertion-ratio (pressure/ratio (or (:zero-assertion-examples summary) 0) example-count)
     :branching-ratio (pressure/ratio (or (:branching-examples summary) 0) example-count)
     :mocking-ratio (pressure/ratio (or (:with-redefs-examples summary) 0) example-count)}))

(defn summary-context
  [summary blocks]
  (let [ratios (summary-ratios summary)]
    {:summary summary
     :blocks blocks
     :file-score (pressure/refactor-pressure-score summary)
     :file-level (pressure/pressure-level summary)
     :remediation-mode (pressure/remediation-mode summary blocks)
     :example-count (or (:example-count summary) 0)
     :harmful-duplication (or (:effective-duplication-score summary) 0)
     :ratios ratios}))
