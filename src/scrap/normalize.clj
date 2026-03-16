(ns scrap.normalize
  (:require [clojure.set :as set]
            [scrap.shared :as shared]))

(declare normalize-shape)

(defn- normalized-map-shape
  [m]
  (->> m
       (map (fn [[k v]]
              [(normalize-shape k) (normalize-shape v)]))
       (sort-by (fn [[k _]] (pr-str k)))
       vec))

(def scalar-normalizers
  [[symbol? (constantly 'sym)]
   [keyword? identity]
   [string? (constantly :string)]
   [number? (constantly :number)]
   [char? (constantly :char)]
   [boolean? (constantly :boolean)]
   [nil? (constantly nil)]])

(defn- normalize-scalar
  [expr]
  (if-let [normalizer (some (fn [[pred f]]
                              (when (pred expr) f))
                            scalar-normalizers)]
    (normalizer expr)
    (type expr)))

(defn normalize-shape
  [expr]
  (cond
    (seq? expr) (apply list (map normalize-shape expr))
    (vector? expr) (mapv normalize-shape expr)
    (map? expr) (normalized-map-shape expr)
    (set? expr) (into #{} (map normalize-shape expr))
    :else (normalize-scalar expr)))

(defn shape-signature
  [expr]
  (pr-str (normalize-shape expr)))

(defn shape-features
  [expr]
  (let [normalized (normalize-shape expr)
        children (cond
                   (seq? normalized) (seq normalized)
                   (map? normalized) (concat (keys normalized) (vals normalized))
                   (coll? normalized) (seq normalized)
                   :else nil)]
    (conj (set (mapcat shape-features children))
          (pr-str normalized))))

(defn collect-large-literal-signatures
  [large-literal? expr]
  (let [children (cond
                   (seq? expr) (seq expr)
                   (map? expr) (concat (keys expr) (vals expr))
                   (coll? expr) (seq expr)
                   :else nil)
        local (when (large-literal? expr)
                [(shape-signature expr)])]
    (vec (concat local (mapcat #(collect-large-literal-signatures large-literal? %) children)))))

(defn form-signatures
  [forms]
  (mapv shape-signature forms))

(defn form-features
  [forms]
  (apply set/union #{} (map shape-features forms)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:01:43.809872-05:00", :module-hash "343609242", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-566558681"} {:id "form/1/declare", :kind "declare", :line 5, :end-line 5, :hash "775804283"} {:id "defn-/normalized-map-shape", :kind "defn-", :line 7, :end-line 13, :hash "1875433455"} {:id "def/scalar-normalizers", :kind "def", :line 15, :end-line 22, :hash "1563872621"} {:id "defn-/normalize-scalar", :kind "defn-", :line 24, :end-line 30, :hash "1400165704"} {:id "defn/normalize-shape", :kind "defn", :line 32, :end-line 39, :hash "541171636"} {:id "defn/shape-signature", :kind "defn", :line 41, :end-line 43, :hash "2002971417"} {:id "defn/shape-features", :kind "defn", :line 45, :end-line 54, :hash "770598821"} {:id "defn/collect-large-literal-signatures", :kind "defn", :line 56, :end-line 65, :hash "-304159590"} {:id "defn/form-signatures", :kind "defn", :line 67, :end-line 69, :hash "589119540"} {:id "defn/form-features", :kind "defn", :line 71, :end-line 73, :hash "-2143902625"}]}
;; clj-mutate-manifest-end
