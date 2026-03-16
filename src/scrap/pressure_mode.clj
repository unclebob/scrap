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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T12:46:06.52612-05:00", :module-hash "-1415008730", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-503703095"} {:id "defn-/split-pressure?", :kind "defn-", :line 5, :end-line 14, :hash "1744232434"} {:id "defn/remediation-mode", :kind "defn", :line 16, :end-line 28, :hash "-641373640"}]}
;; clj-mutate-manifest-end
