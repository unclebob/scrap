(ns scrap.pressure-mode
  (:require [scrap.policy :as policy]
            [scrap.pressure-score :as score]
            [scrap.pressure-stability :as stability]))

(defn- split-pressure?
  [summary]
  (let [{split-avg-scrap :avg-scrap
         split-duplication :effective-duplication-score
         split-subject-repetition :subject-repetition-score} (:split policy/pressure)
        avg-scrap (or (:avg-scrap summary) 0)
        harmful-duplication (or (:effective-duplication-score summary) 0)
        subject-repetition (or (:subject-repetition-score summary) 0)
        helper-hidden (or (:helper-hidden-example-count summary) 0)]
    (or (>= avg-scrap split-avg-scrap)
        (>= harmful-duplication split-duplication)
        (>= subject-repetition split-subject-repetition)
        (pos? helper-hidden))))

(defn remediation-mode
  [summary blocks]
  (let [{split-example-count :example-count
         split-high-pressure-blocks :high-pressure-blocks
         split-max-scrap :max-scrap} (:split policy/pressure)
        example-count (or (:example-count summary) 0)
        high-pressure-blocks (count (filter #(contains? #{"HIGH" "CRITICAL"} (score/pressure-level (:summary %))) blocks))]
    (cond
      (stability/stable-summary? summary) "STABLE"
      (and (not (stability/stable-summary? summary))
           (>= example-count split-example-count)
           (or (>= high-pressure-blocks split-high-pressure-blocks)
               (>= (or (:max-scrap summary) 0) split-max-scrap))
           (split-pressure? summary))
      "SPLIT"
      :else "LOCAL")))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T12:46:06.52612-05:00", :module-hash "-1415008730", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-503703095"} {:id "defn-/split-pressure?", :kind "defn-", :line 5, :end-line 14, :hash "1744232434"} {:id "defn/remediation-mode", :kind "defn", :line 16, :end-line 28, :hash "-641373640"}]}
;; clj-mutate-manifest-end
