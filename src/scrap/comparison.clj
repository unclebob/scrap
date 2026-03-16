(ns scrap.comparison
  (:require [scrap.pressure :as pressure]))

(defn- delta
  [before after]
  (- (or after 0) (or before 0)))

(defn- improved-comparison?
  [comparison]
  (and (<= (:file-score-delta comparison) -5)
       (<= (:harmful-duplication-delta comparison) 0)
       (<= (:max-scrap-delta comparison) 0)))

(defn- helper-regression?
  [comparison]
  (and (pos? (:helper-hidden-delta comparison))
       (>= (:harmful-duplication-delta comparison) 0)
       (<= (:case-matrix-delta comparison) 0)))

(defn- worsening-comparison?
  [comparison]
  (or (pos? (:harmful-duplication-delta comparison))
      (pos? (:max-scrap-delta comparison))
      (>= (:file-score-delta comparison) 5)))

(defn- verdict
  [comparison]
  (cond
    (improved-comparison? comparison) :improved
    (helper-regression? comparison) :worse
    (worsening-comparison? comparison) :worse
    (zero? (:file-score-delta comparison)) :unchanged
    :else :mixed))

(defn compare-reports
  [baseline-doc reports]
  (let [baseline-by-path (into {} (map (juxt :path identity) (:reports baseline-doc)))]
    (mapv
      (fn [report]
        (let [baseline (get baseline-by-path (:path report))
              current-summary (:summary report)
              baseline-summary (:summary baseline)
              comparison (when (and baseline-summary current-summary)
                           (let [file-score-before (pressure/refactor-pressure-score baseline-summary)
                                 file-score-after (pressure/refactor-pressure-score current-summary)
                                 cmp {:baseline-hash (:content-hash baseline)
                                      :current-hash (:content-hash report)
                                      :file-score-delta (delta file-score-before file-score-after)
                                      :avg-scrap-delta (delta (:avg-scrap baseline-summary) (:avg-scrap current-summary))
                                      :max-scrap-delta (delta (:max-scrap baseline-summary) (:max-scrap current-summary))
                                      :harmful-duplication-delta (delta (:harmful-duplication-score baseline-summary)
                                                                        (:harmful-duplication-score current-summary))
                                      :case-matrix-delta (delta (:case-matrix-repetition baseline-summary)
                                                                (:case-matrix-repetition current-summary))
                                      :helper-hidden-delta (delta (:helper-hidden-example-count baseline-summary)
                                                                  (:helper-hidden-example-count current-summary))}]
                             (assoc cmp :verdict (verdict cmp))))]
          (assoc report :comparison comparison)))
      reports)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:46:50.10318-05:00", :module-hash "-584508712", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1627856145"} {:id "defn-/delta", :kind "defn-", :line 4, :end-line 6, :hash "1353036513"} {:id "defn-/improved-comparison?", :kind "defn-", :line 8, :end-line 12, :hash "-1969635699"} {:id "defn-/helper-regression?", :kind "defn-", :line 14, :end-line 18, :hash "-833001903"} {:id "defn-/worsening-comparison?", :kind "defn-", :line 20, :end-line 24, :hash "-1963076733"} {:id "defn-/verdict", :kind "defn-", :line 26, :end-line 33, :hash "185334716"} {:id "defn/compare-reports", :kind "defn", :line 35, :end-line 59, :hash "-2142205739"}]}
;; clj-mutate-manifest-end
