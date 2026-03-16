(ns scrap.shared)

(def assertion-heads
  '#{should should= should-not should-not= should-contain should-not-contain
     should-be-nil should-not-be-nil should-throw})

(def branch-heads
  '#{if if-not when when-not cond case and or try loop while})

(def table-branch-heads
  '#{doseq for every? map mapv run!})

(def setup-heads
  '#{let binding with-redefs before before-all around with with-stubs})

(def helper-def-heads
  '#{defn defn- defmacro})

(def speclj-form-symbols
  '#{describe context it before before-all after with-stubs with around run-specs})

(def speclj-forms
  (set (map name speclj-form-symbols)))

(def default-top-example-count 5)
(def default-top-block-count 3)
(def duplication-threshold 0.5)
(def baseline-version 1)
(def complexity-cap 25.0)
(def complexity-rise-rate 0.18)
(def trivial-complexity-floor 1.0)
