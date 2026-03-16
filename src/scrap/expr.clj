(ns scrap.expr
  (:require [clojure.string :as str]
            [scrap.shared :as shared]))

(defn seq-head [expr]
  (when (seq? expr)
    (first expr)))

(defn- symbol-name [x]
  (when (symbol? x)
    (name x)))

(defn namespaced-symbol?
  [x]
  (and (symbol? x) (namespace x)))

(defn large-literal?
  [expr]
  (or (and (string? expr) (> (count (str/split-lines expr)) 5))
      (and (map? expr) (> (count expr) 10))
      (and (vector? expr) (> (count expr) 10))
      (and (set? expr) (> (count expr) 10))))

(defn temp-resource-call?
  [head]
  (let [n (symbol-name head)]
    (or (#{"createTempFile" "createTempDirectory" "mkdir" "mkdirs" "future"} n)
        (#{"sh" "slurp" "spit"} n)
        (and n (str/includes? n "Temp")))))

(defn control-symbol?
  [sym]
  (or (contains? shared/assertion-heads sym)
      (contains? shared/setup-heads sym)
      (contains? shared/branch-heads sym)
      (contains? shared/table-branch-heads sym)
      (contains? shared/speclj-form-symbols sym)
      (= 'fn sym)
      (= 'fn* sym)
      (= 'quote sym)
      (= 'var sym)
      (= '. sym)))

(defn large-case-table?
  [expr]
  (and (or (vector? expr) (list? expr))
       (>= (count expr) 2)
       (every? coll? expr)))

(defn child-nodes
  [expr]
  (cond
    (seq? expr) (seq expr)
    (map? expr) (concat (keys expr) (vals expr))
    (coll? expr) (seq expr)
    :else nil))

(defn setup-forms
  [forms]
  (filterv #(contains? shared/setup-heads (seq-head %)) forms))
