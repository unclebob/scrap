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
;; {:version 1, :tested-at "2026-03-16T14:32:06.711458-05:00", :module-hash "1589164245", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-894833977"} {:id "defn-/size-factor", :kind "defn-", :line 5, :end-line 10, :hash "1081562012"} {:id "defn-/score-terms", :kind "defn-", :line 12, :end-line 27, :hash "-2088524616"} {:id "defn/refactor-pressure-score", :kind "defn", :line 29, :end-line 34, :hash "-1986702854"} {:id "defn/pressure-level", :kind "defn", :line 36, :end-line 45, :hash "1993636688"}]}
;; clj-mutate-manifest-end
