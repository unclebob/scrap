(ns scrap.policy)

(def report
  {:top-example-count 5
   :top-block-count 3
   :baseline-version 1})

(def duplication
  {:threshold 0.5
   :matrix-max-scrap 18
   :matrix-max-lines 12
   :matrix-max-assertions 1
   :matrix-max-branches 0
   :matrix-max-setup-depth 2
   :matrix-max-with-redefs 0
   :matrix-max-temp-resources 0
   :matrix-max-helper-hidden-lines 0
   :matrix-max-subject-symbols 2
   :coverage-credit-divisor 2})

(def complexity
  {:cap 25.0
   :rise-rate 0.18
   :floor 1.0})

(def pressure
  {:size-factors [{:up-to 1 :factor 0.25}
                  {:up-to 2 :factor 0.40}
                  {:up-to 4 :factor 0.65}
                  {:up-to nil :factor 1.0}]
   :weights {:avg-scrap 1.2
             :max-scrap 0.6
             :effective-duplication-score 0.8
             :low-assertion-ratio 20
             :branching-ratio 15
             :with-redefs-ratio 15
             :helper-hidden-ratio 12}
   :matrix-credit 1.5
   :levels {:critical 55
            :high 35
            :medium 18}
   :split {:avg-scrap 10
           :effective-duplication-score 20
           :subject-repetition-score 12
           :example-count 12
           :high-pressure-blocks 2
           :max-scrap 35}})

(def actionability
  {:matrix {:min-case-matrix-repetition 2
            :effective-duplication-divisor 3
            :max-scrap 12
            :max-branching-ratio 0.15
            :max-mocking-ratio 0.2}
   :local {:low-assertion-ratio 0.4
           :max-branching-ratio 0.3
           :max-mocking-ratio 0.35
           :max-scrap 20
           :avg-scrap 12}
   :max-actions 4})
