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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:04:10.375952-05:00", :module-hash "-202445204", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "832976674"} {:id "defn-/size-factor", :kind "defn-", :line 4, :end-line 11, :hash "1104590160"} {:id "defn-/score-terms", :kind "defn-", :line 13, :end-line 21, :hash "-927006487"} {:id "defn/refactor-pressure-score", :kind "defn", :line 23, :end-line 28, :hash "-1906857607"} {:id "defn/pressure-level", :kind "defn", :line 30, :end-line 38, :hash "-2113533511"}]}
;; clj-mutate-manifest-end
