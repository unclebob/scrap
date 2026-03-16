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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T14:29:44.615243-05:00", :module-hash "1010556448", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-469076491"} {:id "defn/summary-ratios", :kind "defn", :line 4, :end-line 10, :hash "894634528"} {:id "defn/summary-context", :kind "defn", :line 12, :end-line 22, :hash "-1886115800"}]}
;; clj-mutate-manifest-end
