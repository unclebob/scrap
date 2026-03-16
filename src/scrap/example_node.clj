(ns scrap.example-node
  (:require [scrap.example-helpers :as helpers]
            [scrap.expr :as expr]
            [scrap.shared :as shared]))

(declare analyze-node)
(declare helper-expanded-metrics)

(defn- table-driven-form?
  [expr]
  (let [head (expr/seq-head expr)
        children (expr/child-nodes expr)]
    (or (and (contains? shared/table-branch-heads head)
             (some expr/large-case-table? children))
        (and (contains? shared/assertion-heads head)
             (some expr/large-case-table? children))
        (some table-driven-form? children))))

(defn- helper-call?
  [head helper-defs]
  (and (symbol? head) (contains? helper-defs head)))

(defn- next-setup-depth
  [head setup-depth]
  (if (contains? shared/setup-heads head)
    (inc setup-depth)
    setup-depth))

(defn- subject-symbols-for-head
  [head helper-call?]
  (cond
    (expr/namespaced-symbol? head) #{head}
    (and (symbol? head)
         (not helper-call?)
         (not (expr/control-symbol? head)))
    #{head}
    :else #{}))

(defn- local-node-metrics
  [expr head helper-call? setup-depth]
  {:assertions (if (contains? shared/assertion-heads head) 1 0)
   :branches (if (contains? shared/branch-heads head) 1 0)
   :table-branches (if (contains? shared/table-branch-heads head) 1 0)
   :with-redefs (if (= 'with-redefs head) 1 0)
   :helper-calls (if helper-call? 1 0)
   :helper-hidden-lines 0
   :temp-resources (if (expr/temp-resource-call? head) 1 0)
   :large-literals (if (expr/large-literal? expr) 1 0)
   :max-setup-depth setup-depth
   :subject-symbols (subject-symbols-for-head head helper-call?)
   :table-driven? (table-driven-form? expr)})

(defn analyze-node
  [expr helper-context setup-depth helper-stack]
  (let [{:keys [helper-defs]} helper-context
        head (expr/seq-head expr)
        helper-call? (helper-call? head helper-defs)
        next-depth (next-setup-depth head setup-depth)
        child-metrics (map #(analyze-node % helper-context next-depth helper-stack) (expr/child-nodes expr))
        helper-metrics (if (and helper-call? (not (contains? helper-stack head)))
                         (helper-expanded-metrics helper-context head (conj helper-stack head) setup-depth)
                         nil)
        local (local-node-metrics expr head helper-call? setup-depth)]
    (apply helpers/combine-metrics local (concat child-metrics (when helper-metrics [helper-metrics])))))

(defn helper-expanded-metrics
  [{:keys [helper-defs helper-cache] :as helper-context} helper-sym helper-stack setup-depth]
  (if-let [cached (get @helper-cache [helper-sym setup-depth])]
    cached
    (let [body (get helper-defs helper-sym)
          metrics (if (seq body)
                    (apply helpers/combine-metrics
                           {:helper-hidden-lines (helpers/helper-line-span body)}
                           (map #(analyze-node % helper-context setup-depth helper-stack) body))
                    {:helper-hidden-lines 0})]
      (swap! helper-cache assoc [helper-sym setup-depth] metrics)
      metrics)))

(defn top-level-phase
  [expr helper-context]
  (let [head (expr/seq-head expr)
        metrics (analyze-node expr helper-context 0 #{})]
    (cond
      (contains? shared/setup-heads head) :setup
      (pos? (:assertions metrics)) :assert
      :else :action)))

(defn assert-forms
  [forms helper-context]
  (filterv #(= :assert (top-level-phase % helper-context)) forms))

(defn arrange-forms
  [forms helper-context]
  (filterv #(not= :assert (top-level-phase % helper-context)) forms))

(defn assertion-clusters
  [body helper-context]
  (->> body
       (map #(top-level-phase % helper-context))
       (partition-by identity)
       (filter #(= :assert (first %)))
       count))
