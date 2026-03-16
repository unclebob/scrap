(ns scrap.pressure-score
  (:require [scrap.policy :as policy]
            [scrap.pressure-stability :as stability]))

(defn- size-factor
  [summary]
  (let [example-count (or (:example-count summary) 0)]
    (:factor (first (filter #(or (nil? (:up-to %))
                                 (<= example-count (:up-to %)))
                            (:size-factors policy/pressure))))))

(defn- score-terms
  [summary example-count]
  (let [{:keys [avg-scrap
                max-scrap
                effective-duplication-score
                low-assertion-ratio
                branching-ratio
                with-redefs-ratio
                helper-hidden-ratio]} (:weights policy/pressure)]
    [(* avg-scrap (or (:avg-scrap summary) 0))
     (* max-scrap (or (:max-scrap summary) 0))
     (* effective-duplication-score (or (:effective-duplication-score summary) 0))
     (* low-assertion-ratio (stability/ratio (or (:low-assertion-examples summary) 0) example-count))
     (* branching-ratio (stability/ratio (or (:branching-examples summary) 0) example-count))
     (* with-redefs-ratio (stability/ratio (or (:with-redefs-examples summary) 0) example-count))
     (* helper-hidden-ratio (stability/ratio (or (:helper-hidden-example-count summary) 0) example-count))]))

(defn refactor-pressure-score
  [summary]
  (let [example-count (or (:example-count summary) 0)
        base (reduce + (score-terms summary example-count))
        matrix-credit (* (:matrix-credit policy/pressure) (or (:case-matrix-repetition summary) 0))]
    (max 0 (- (* (size-factor summary) base) matrix-credit))))

(defn pressure-level
  [summary]
  (let [score (refactor-pressure-score summary)
        {:keys [critical high medium]} (:levels policy/pressure)]
    (cond
      (stability/stable-summary? summary) "STABLE"
      (>= score critical) "CRITICAL"
      (>= score high) "HIGH"
      (>= score medium) "MEDIUM"
      :else "LOW")))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:04:10.375952-05:00", :module-hash "-202445204", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "832976674"} {:id "defn-/size-factor", :kind "defn-", :line 4, :end-line 11, :hash "1104590160"} {:id "defn-/score-terms", :kind "defn-", :line 13, :end-line 21, :hash "-927006487"} {:id "defn/refactor-pressure-score", :kind "defn", :line 23, :end-line 28, :hash "-1906857607"} {:id "defn/pressure-level", :kind "defn", :line 30, :end-line 38, :hash "-2113533511"}]}
;; clj-mutate-manifest-end
