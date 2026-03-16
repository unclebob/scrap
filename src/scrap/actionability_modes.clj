(ns scrap.actionability-modes
  (:require [scrap.pressure :as pressure]))

(defn- matrix-heavy?
  [summary max-scrap branching-ratio mocking-ratio]
  (let [coverage-matrix-candidates (or (:coverage-matrix-candidates summary) 0)
        case-matrix-repetition (or (:case-matrix-repetition summary) 0)
        harmful-duplication (or (:effective-duplication-score summary) 0)]
    (and (pos? coverage-matrix-candidates)
         (>= case-matrix-repetition (max 2 (quot harmful-duplication 3)))
         (<= max-scrap 12)
         (<= branching-ratio 0.15)
         (< mocking-ratio 0.2))))

(defn- local-safe?
  [mode summary zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio]
  (let [harmful-duplication (or (:effective-duplication-score summary) 0)
        max-scrap (or (:max-scrap summary) 0)]
    (and (= mode "LOCAL")
         (or (> harmful-duplication 0)
             (> zero-assertion-ratio 0.0)
             (> low-assertion-ratio 0.4)
             (> max-scrap 20))
         (<= branching-ratio 0.3)
         (< mocking-ratio 0.35))))

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
        example-count (or (:example-count summary) 0)
        max-scrap (or (:max-scrap summary) 0)
        low-assertion-ratio (pressure/ratio (or (:low-assertion-examples summary) 0) example-count)
        zero-assertion-ratio (pressure/ratio (or (:zero-assertion-examples summary) 0) example-count)
        branching-ratio (pressure/ratio (or (:branching-examples summary) 0) example-count)
        mocking-ratio (pressure/ratio (or (:with-redefs-examples summary) 0) example-count)]
    (or (stable-action mode)
        (matrix-action summary max-scrap branching-ratio mocking-ratio)
        (local-action mode summary zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio)
        (split-action mode)
        {:mode "REVIEW_FIRST"
         :message "Do not auto-refactor immediately; inspect the file shape before acting on the recommendations."})))
