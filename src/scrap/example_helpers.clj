(ns scrap.example-helpers
  (:require [clojure.set :as set]
            [scrap.expr :as expr]
            [scrap.shared :as shared]))

(defn combine-metrics
  [& metrics]
  (reduce
    (fn [acc metric]
      {:assertions (+ (:assertions acc 0) (:assertions metric 0))
       :branches (+ (:branches acc 0) (:branches metric 0))
       :table-branches (+ (:table-branches acc 0) (:table-branches metric 0))
       :with-redefs (+ (:with-redefs acc 0) (:with-redefs metric 0))
       :helper-calls (+ (:helper-calls acc 0) (:helper-calls metric 0))
       :helper-hidden-lines (+ (:helper-hidden-lines acc 0) (:helper-hidden-lines metric 0))
       :temp-resources (+ (:temp-resources acc 0) (:temp-resources metric 0))
       :large-literals (+ (:large-literals acc 0) (:large-literals metric 0))
       :max-setup-depth (max (:max-setup-depth acc 0) (:max-setup-depth metric 0))
       :subject-symbols (set/union (:subject-symbols acc #{}) (:subject-symbols metric #{}))
       :table-driven? (or (:table-driven? acc false) (:table-driven? metric false))})
    {:assertions 0
     :branches 0
     :table-branches 0
     :with-redefs 0
     :helper-calls 0
     :helper-hidden-lines 0
     :temp-resources 0
     :large-literals 0
     :max-setup-depth 0
     :subject-symbols #{}
     :table-driven? false}
    metrics))

(defn non-helper-body
  [form]
  (let [[_ _ & more] form]
    (cond
      (string? (first more)) (recur (cons nil (cons nil (rest more))))
      (map? (first more)) (recur (cons nil (cons nil (rest more))))
      :else more)))

(defn- collect-helper-defs
  [forms]
  (letfn [(walk [form]
            (let [head (expr/seq-head form)
                  nested (cond
                           (seq? form) (mapcat walk (seq form))
                           (map? form) (mapcat walk (concat (keys form) (vals form)))
                           (coll? form) (mapcat walk form)
                           :else [])]
              (if (and (contains? shared/helper-def-heads head)
                       (symbol? (second form)))
                (cons [(second form) (non-helper-body form)] nested)
                nested)))]
    (into {} (mapcat walk forms))))

(defn helper-line-span
  [body]
  (reduce
    (fn [span form]
      (+ span
         (max 1 (inc (- (or (-> form meta :end-line) (-> form meta :line) 1)
                        (or (-> form meta :line) 1))))))
    0
    body))

(defn helper-context
  [forms]
  {:helper-defs (collect-helper-defs forms)
   :helper-cache (atom {})})
