(ns scrap.example-score
  (:require [scrap.example-helpers :as helpers]
            [scrap.example-metrics :as metrics]
            [scrap.example-node :as node]
            [scrap.example-shapes :as shapes]
            [scrap.example-smells :as smells]
            [scrap.shared :as shared]))

(defn score-example
  [it-form helper-context inherited-setup-forms describe-path]
  (let [[_ name & body] it-form
        inherited-setup (count inherited-setup-forms)
        metrics (apply helpers/combine-metrics (map #(node/analyze-node % helper-context inherited-setup #{}) body))
        raw-lines (shapes/raw-line-count it-form)
        line-count (+ raw-lines (:helper-hidden-lines metrics))
        shape-data (shapes/example-form-shapes body helper-context inherited-setup-forms)
        literal-features (set (:literal-signatures shape-data))
        phases (node/assertion-clusters body helper-context)
        api-contract? (metrics/api-contract-example? metrics line-count phases)
        scored-setup-depth (metrics/scored-setup-depth metrics api-contract?)
        complexity (+ 1
                      (:branches metrics)
                      scored-setup-depth
                      (:helper-calls metrics)
                      (quot (:helper-hidden-lines metrics) 8))
        table-driven? (:table-driven? metrics)
        smell-entries (smells/smell-entries metrics line-count phases table-driven? api-contract?)
        smell-penalty (reduce + (map :penalty smell-entries))
        scored-branch-penalty (metrics/scored-branch-penalty metrics table-driven? api-contract?)
        complexity-score (metrics/saturating-complexity-score (+ 1 scored-branch-penalty scored-setup-depth (:helper-calls metrics)))
        scrap (+ complexity-score smell-penalty)]
    {:name name
     :describe-path describe-path
     :line (or (-> it-form meta :line) 1)
     :line-count line-count
     :raw-line-count raw-lines
     :assertions (:assertions metrics)
     :branches (+ (:branches metrics) (:table-branches metrics))
     :setup-depth (:max-setup-depth metrics)
     :with-redefs (:with-redefs metrics)
     :helper-calls (:helper-calls metrics)
     :helper-hidden-lines (:helper-hidden-lines metrics)
     :temp-resources (:temp-resources metrics)
     :table-driven? table-driven?
     :api-contract? api-contract?
     :complexity complexity
     :complexity-score complexity-score
     :subject-symbols (:subject-symbols metrics)
     :assert-signatures (:assert-signatures shape-data)
     :assert-features (:assert-features shape-data)
     :setup-signatures (:setup-signatures shape-data)
     :setup-features (:setup-features shape-data)
     :fixture-shape (:fixture-shape shape-data)
     :fixture-features (:fixture-features shape-data)
     :arrange-signatures (:arrange-signatures shape-data)
     :arrange-features (:arrange-features shape-data)
     :literal-signatures (:literal-signatures shape-data)
     :literal-features literal-features
     :scrap scrap
     :smells (mapv :label smell-entries)}))
