(ns scrap.actionability-rules)

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
  [(recommendation-rule
     (pos? (or (:coverage-matrix-candidates summary) 0))
     (recommendation 3 "Convert repeated low-complexity examples into table-driven checks; treat this as coverage-matrix repetition, not harmful duplication."))
   (recommendation-rule
     (or (> zero-assertion-ratio 0.0) (> low-assertion-ratio 0.4))
     (recommendation 3 "Strengthen assertions in weak examples before doing structural cleanup."))
   (recommendation-rule
     (and (= mode "LOCAL") (> (or (:max-scrap summary) 0) 20))
     (recommendation 3 "Split oversized examples into narrower examples."))
   (recommendation-rule
     (> harmful-duplication 0)
     (recommendation 2 "Extract shared setup or repeated assertion scaffolding only where harmful duplication is dominating."))
   (recommendation-rule
     (> mocking-ratio 0.3)
     (recommendation 2 "Reduce mocking and move coverage toward higher-level behaviors."))
   (recommendation-rule
     (> branching-ratio 0.3)
     (recommendation 2 "Remove logic from specs or keep variation in explicit data tables rather than control flow."))
   (recommendation-rule
     (> (or (:helper-hidden-example-count summary) 0) 0)
     (recommendation 1 "Be skeptical of helper extraction that only hides setup; helper-hidden complexity should still count as complexity."))
   (recommendation-rule
     (and (= mode "LOCAL") (> (or (:avg-scrap summary) 0) 12))
     (recommendation 1 "Consider splitting this file or block by responsibility."))])

(defn split-rule
  [mode]
  (recommendation-rule
    (= mode "SPLIT")
    (recommendation 3 "Split this spec file by responsibility before attempting local cleanup; the structural pressure is spread across multiple hotspots.")))
