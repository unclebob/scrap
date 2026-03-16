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
