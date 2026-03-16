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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:59:29.483691-05:00", :module-hash "907324294", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-1539827596"} {:id "defn/seq-head", :kind "defn", :line 5, :end-line 7, :hash "-1387101079"} {:id "defn-/symbol-name", :kind "defn-", :line 9, :end-line 11, :hash "-350616189"} {:id "defn/namespaced-symbol?", :kind "defn", :line 13, :end-line 15, :hash "1386266118"} {:id "defn/large-literal?", :kind "defn", :line 17, :end-line 22, :hash "-404200277"} {:id "defn/temp-resource-call?", :kind "defn", :line 24, :end-line 29, :hash "-958219488"} {:id "defn/control-symbol?", :kind "defn", :line 31, :end-line 42, :hash "1389660570"} {:id "defn/large-case-table?", :kind "defn", :line 44, :end-line 48, :hash "-1476672665"} {:id "defn/child-nodes", :kind "defn", :line 50, :end-line 56, :hash "1447842744"} {:id "defn/setup-forms", :kind "defn", :line 58, :end-line 60, :hash "-408533626"}]}
;; clj-mutate-manifest-end
