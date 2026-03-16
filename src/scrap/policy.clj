(ns scrap.policy
  (:require [clojure.spec.alpha :as s]))

(def report-policy
  {:top-example-count 5
   :top-block-count 3
   :baseline-version 1})

(def duplication-policy
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

(def complexity-policy
  {:cap 25.0
   :rise-rate 0.18
   :floor 1.0})

(def pressure-policy
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

(def actionability-policy
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

;; Backward-compatible aliases for internal callers.
(def report report-policy)
(def duplication duplication-policy)
(def complexity complexity-policy)
(def pressure pressure-policy)
(def actionability actionability-policy)

(s/def ::positive-int (s/and int? pos?))
(s/def ::non-negative-int (s/and int? (complement neg?)))
(s/def ::non-negative-number (s/and number? (complement neg?)))
(s/def ::ratio (s/and number? #(<= 0 % 1)))
(s/def ::top-example-count ::positive-int)
(s/def ::top-block-count ::positive-int)
(s/def ::baseline-version ::positive-int)
(s/def ::report-policy (s/keys :req-un [::top-example-count ::top-block-count ::baseline-version]))

(s/def ::threshold ::ratio)
(s/def ::matrix-max-scrap ::non-negative-number)
(s/def ::matrix-max-lines ::positive-int)
(s/def ::matrix-max-assertions ::non-negative-int)
(s/def ::matrix-max-branches ::non-negative-int)
(s/def ::matrix-max-setup-depth ::non-negative-int)
(s/def ::matrix-max-with-redefs ::non-negative-int)
(s/def ::matrix-max-temp-resources ::non-negative-int)
(s/def ::matrix-max-helper-hidden-lines ::non-negative-int)
(s/def ::matrix-max-subject-symbols ::positive-int)
(s/def ::coverage-credit-divisor ::positive-int)
(s/def ::duplication-policy
  (s/keys :req-un [::threshold
                   ::matrix-max-scrap
                   ::matrix-max-lines
                   ::matrix-max-assertions
                   ::matrix-max-branches
                   ::matrix-max-setup-depth
                   ::matrix-max-with-redefs
                   ::matrix-max-temp-resources
                   ::matrix-max-helper-hidden-lines
                   ::matrix-max-subject-symbols
                   ::coverage-credit-divisor]))

(s/def ::cap ::non-negative-number)
(s/def ::rise-rate ::non-negative-number)
(s/def ::floor ::non-negative-number)
(s/def ::complexity-policy (s/keys :req-un [::cap ::rise-rate ::floor]))

(s/def ::up-to (s/nilable ::positive-int))
(s/def ::factor ::ratio)
(s/def ::size-factor (s/keys :req-un [::up-to ::factor]))
(s/def ::avg-scrap ::non-negative-number)
(s/def ::max-scrap ::non-negative-number)
(s/def ::effective-duplication-score ::non-negative-number)
(s/def ::low-assertion-ratio ::non-negative-number)
(s/def ::branching-ratio ::non-negative-number)
(s/def ::with-redefs-ratio ::non-negative-number)
(s/def ::helper-hidden-ratio ::non-negative-number)
(s/def ::weights
  (s/keys :req-un [::avg-scrap
                   ::max-scrap
                   ::effective-duplication-score
                   ::low-assertion-ratio
                   ::branching-ratio
                   ::with-redefs-ratio
                   ::helper-hidden-ratio]))
(s/def ::matrix-credit ::non-negative-number)
(s/def ::critical ::non-negative-number)
(s/def ::high ::non-negative-number)
(s/def ::medium ::non-negative-number)
(s/def ::levels (s/keys :req-un [::critical ::high ::medium]))
(s/def ::subject-repetition-score ::non-negative-number)
(s/def ::example-count ::positive-int)
(s/def ::high-pressure-blocks ::positive-int)
(s/def ::split
  (s/keys :req-un [::avg-scrap
                   ::effective-duplication-score
                   ::subject-repetition-score
                   ::example-count
                   ::high-pressure-blocks
                   ::max-scrap]))
(s/def ::size-factors (s/coll-of ::size-factor :kind vector? :min-count 1))
(s/def ::pressure-policy (s/keys :req-un [::size-factors ::weights ::matrix-credit ::levels ::split]))

(s/def ::min-case-matrix-repetition ::positive-int)
(s/def ::effective-duplication-divisor ::positive-int)
(s/def ::max-branching-ratio ::ratio)
(s/def ::max-mocking-ratio ::ratio)
(s/def ::matrix (s/keys :req-un [::min-case-matrix-repetition
                                 ::effective-duplication-divisor
                                 ::max-scrap
                                 ::max-branching-ratio
                                 ::max-mocking-ratio]))
(s/def ::avg-scrap-threshold ::non-negative-number)
(s/def ::local (s/keys :req-un [::low-assertion-ratio
                                ::max-branching-ratio
                                ::max-mocking-ratio
                                ::max-scrap
                                ::avg-scrap]))
(s/def ::max-actions ::positive-int)
(s/def ::actionability-policy (s/keys :req-un [::matrix ::local ::max-actions]))

(defn- validate!
  [spec value label]
  (when-not (s/valid? spec value)
    (throw (ex-info (str "Invalid policy: " label)
                    {:label label
                     :explain (s/explain-data spec value)})))
  value)

(validate! ::report-policy report-policy "report-policy")
(validate! ::duplication-policy duplication-policy "duplication-policy")
(validate! ::complexity-policy complexity-policy "complexity-policy")
(validate! ::pressure-policy pressure-policy "pressure-policy")
(validate! ::actionability-policy actionability-policy "actionability-policy")
