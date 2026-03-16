(ns scrap.actionability-rules
  (:require [scrap.policy :as policy]))

(defn- recommendation
  [confidence text]
  {:confidence confidence
   :label ({3 "HIGH" 2 "MEDIUM" 1 "LOW"} confidence)
   :text text})

(defn- recommendation-rule
  [enabled? recommendation]
  (when enabled? recommendation))

(defn local-action-rules
  [summary mode zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio harmful-duplication]
  (let [{local-low-assertion-ratio :low-assertion-ratio
         local-max-scrap :max-scrap
         local-avg-scrap :avg-scrap
         local-max-mocking-ratio :max-mocking-ratio
         local-max-branching-ratio :max-branching-ratio} (:local policy/actionability)]
    [(recommendation-rule
       (pos? (or (:coverage-matrix-candidates summary) 0))
       (recommendation 3 "Convert repeated low-complexity examples into table-driven checks; treat this as coverage-matrix repetition, not harmful duplication."))
     (recommendation-rule
       (or (> zero-assertion-ratio 0.0) (> low-assertion-ratio local-low-assertion-ratio))
       (recommendation 3 "Strengthen assertions in weak examples before doing structural cleanup."))
     (recommendation-rule
       (and (= mode "LOCAL") (> (or (:max-scrap summary) 0) local-max-scrap))
       (recommendation 3 "Split oversized examples into narrower examples."))
     (recommendation-rule
       (> harmful-duplication 0)
       (recommendation 2 "Extract shared setup or repeated assertion scaffolding only where harmful duplication is dominating."))
     (recommendation-rule
       (> mocking-ratio local-max-mocking-ratio)
       (recommendation 2 "Reduce mocking and move coverage toward higher-level behaviors."))
     (recommendation-rule
       (> branching-ratio local-max-branching-ratio)
       (recommendation 2 "Remove logic from specs or keep variation in explicit data tables rather than control flow."))
     (recommendation-rule
       (> (or (:helper-hidden-example-count summary) 0) 0)
       (recommendation 1 "Be skeptical of helper extraction that only hides setup; helper-hidden complexity should still count as complexity."))
     (recommendation-rule
       (and (= mode "LOCAL") (> (or (:avg-scrap summary) 0) local-avg-scrap))
       (recommendation 1 "Consider splitting this file or block by responsibility."))]))

(defn split-rule
  [mode]
  (recommendation-rule
    (= mode "SPLIT")
    (recommendation 3 "Split this spec file by responsibility before attempting local cleanup; the structural pressure is spread across multiple hotspots.")))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:40:03.147812-05:00", :module-hash "-1229084841", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "842889423"} {:id "defn-/recommendation", :kind "defn-", :line 3, :end-line 7, :hash "1747581973"} {:id "defn-/recommendation-rule", :kind "defn-", :line 9, :end-line 11, :hash "-1618343081"} {:id "defn/local-action-rules", :kind "defn", :line 13, :end-line 38, :hash "-824743888"} {:id "defn/split-rule", :kind "defn", :line 40, :end-line 44, :hash "-1969061462"}]}
;; clj-mutate-manifest-end
