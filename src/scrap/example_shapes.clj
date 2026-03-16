(ns scrap.example-shapes
  (:require [scrap.example-node :as node]
            [scrap.expr :as expr]
            [scrap.normalize :as normalize]))

(defn raw-line-count
  [form]
  (max 1 (inc (- (or (-> form meta :end-line) (-> form meta :line) 1)
                 (or (-> form meta :line) 1)))))

(defn example-form-shapes
  [body helper-context inherited-setup-forms]
  (let [local-setup (expr/setup-forms body)
        effective-setup (vec (concat inherited-setup-forms local-setup))
        assert-forms* (node/assert-forms body helper-context)
        arrange-forms* (node/arrange-forms body helper-context)
        setup-signatures (normalize/form-signatures effective-setup)]
    {:assert-signatures (normalize/form-signatures assert-forms*)
     :assert-features (normalize/form-features assert-forms*)
     :setup-signatures setup-signatures
     :setup-features (normalize/form-features effective-setup)
     :arrange-signatures (normalize/form-signatures arrange-forms*)
     :arrange-features (normalize/form-features arrange-forms*)
     :literal-signatures (vec (mapcat #(normalize/collect-large-literal-signatures expr/large-literal? %) body))
     :fixture-shape (when (seq setup-signatures) (normalize/shape-signature setup-signatures))
     :fixture-features (if (seq setup-signatures)
                        (normalize/shape-features setup-signatures)
                        #{})}))
