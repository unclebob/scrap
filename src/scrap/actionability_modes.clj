(ns scrap.actionability-modes
  (:require [scrap.judgment :as judgment]
            [scrap.policy :as policy]
            [scrap.pressure :as pressure]))

(defn- matrix-heavy?
  [summary max-scrap branching-ratio mocking-ratio]
  (let [{min-case-matrix-repetition :min-case-matrix-repetition
         effective-duplication-divisor :effective-duplication-divisor
         matrix-max-scrap :max-scrap
         max-branching-ratio :max-branching-ratio
         max-mocking-ratio :max-mocking-ratio} (:matrix policy/actionability)
        coverage-matrix-candidates (or (:coverage-matrix-candidates summary) 0)
        case-matrix-repetition (or (:case-matrix-repetition summary) 0)
        harmful-duplication (or (:effective-duplication-score summary) 0)]
    (and (pos? coverage-matrix-candidates)
         (>= case-matrix-repetition (max min-case-matrix-repetition
                                         (quot harmful-duplication effective-duplication-divisor)))
         (<= max-scrap matrix-max-scrap)
         (<= branching-ratio max-branching-ratio)
         (< mocking-ratio max-mocking-ratio))))

(defn- local-safe?
  [mode summary zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio]
  (let [{local-low-assertion-ratio :low-assertion-ratio
         local-max-branching-ratio :max-branching-ratio
         local-max-mocking-ratio :max-mocking-ratio
         local-max-scrap :max-scrap} (:local policy/actionability)
        harmful-duplication (or (:effective-duplication-score summary) 0)
        max-scrap (or (:max-scrap summary) 0)]
    (and (= mode "LOCAL")
         (or (> harmful-duplication 0)
             (> zero-assertion-ratio 0.0)
             (> low-assertion-ratio local-low-assertion-ratio)
             (> max-scrap local-max-scrap))
         (<= branching-ratio local-max-branching-ratio)
         (< mocking-ratio local-max-mocking-ratio))))

(defn- stable-action
  [mode]
  (when (= mode "STABLE")
    {:mode "LEAVE_ALONE"
     :message "Leave this file alone unless explicitly requested; the current structure is stable enough."}))

(defn- matrix-action
  [summary max-scrap branching-ratio mocking-ratio]
  (when (matrix-heavy? summary max-scrap branching-ratio mocking-ratio)
    {:mode "AUTO_TABLE_DRIVE"
     :message "Safe to table-drive automatically; repetition looks like coverage-matrix structure rather than harmful design."}))

(defn- local-action
  [mode summary zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio]
  (when (local-safe? mode summary zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio)
    {:mode "AUTO_REFACTOR"
     :message "Safe to refactor automatically with local changes; focus on assertions, oversized examples, and duplicated scaffolding."}))

(defn- split-action
  [mode]
  (when (= mode "SPLIT")
    {:mode "MANUAL_SPLIT"
     :message "Do not auto-refactor locally first; split the file by responsibility before smaller cleanup."}))

(defn ai-actionability
  [summary blocks]
  (let [mode (pressure/remediation-mode summary blocks)
        max-scrap (or (:max-scrap summary) 0)
        {:keys [low-assertion-ratio zero-assertion-ratio branching-ratio mocking-ratio]}
        (judgment/summary-ratios summary)]
    (or (stable-action mode)
        (matrix-action summary max-scrap branching-ratio mocking-ratio)
        (local-action mode summary zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio)
        (split-action mode)
        {:mode "REVIEW_FIRST"
         :message "Do not auto-refactor immediately; inspect the file shape before acting on the recommendations."})))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:38:40.297184-05:00", :module-hash "212986285", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-2024276655"} {:id "defn-/matrix-heavy?", :kind "defn-", :line 4, :end-line 13, :hash "-1414353544"} {:id "defn-/local-safe?", :kind "defn-", :line 15, :end-line 25, :hash "-2108573556"} {:id "defn-/stable-action", :kind "defn-", :line 27, :end-line 31, :hash "-808541417"} {:id "defn-/matrix-action", :kind "defn-", :line 33, :end-line 37, :hash "-2087819871"} {:id "defn-/local-action", :kind "defn-", :line 39, :end-line 43, :hash "2132325172"} {:id "defn-/split-action", :kind "defn-", :line 45, :end-line 49, :hash "1052186021"} {:id "defn/ai-actionability", :kind "defn", :line 51, :end-line 65, :hash "-2143284383"}]}
;; clj-mutate-manifest-end
