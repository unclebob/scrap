(ns scrap.summary
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [scrap.shared :as shared]))

(defn- jaccard-similarity
  [a b]
  (let [a (or a #{})
        b (or b #{})
        union-size (count (set/union a b))]
    (if (zero? union-size)
      0.0
      (/ (count (set/intersection a b))
         union-size))))

(defn- similar-example-count
  [examples key-fn]
  (count
    (filter
      (fn [example]
        (let [features (key-fn example)]
          (and (seq features)
               (some #(>= (jaccard-similarity features (key-fn %)) shared/duplication-threshold)
                     (remove #{example} examples)))))
      examples)))

(defn- average-similarity
  [examples key-fn]
  (let [pairs (for [[idx left] (map-indexed vector examples)
                    right (drop (inc idx) examples)]
                (jaccard-similarity (key-fn left) (key-fn right)))]
    (if (seq pairs)
      (/ (reduce + pairs) (count pairs))
      0.0)))

(defn- distinct-shape-count
  [examples key-fn]
  (count (distinct (remove nil? (mapcat key-fn examples)))))

(defn- coverage-matrix-candidate?
  [example]
  (and (<= (:scrap example) 18)
       (<= (:line-count example) 12)
       (<= (:assertions example) 1)
       (<= (:branches example) 0)
       (<= (:setup-depth example) 2)
       (<= (:with-redefs example) 0)
       (<= (or (:temp-resources example) 0) 0)
       (<= (or (:helper-hidden-lines example) 0) 0)
       (or (:table-driven? example)
           (and (<= (count (:subject-symbols example)) 2)
                (or (seq (:assert-features example))
                    (seq (:arrange-features example)))))))

(defn- similar-to-any?
  [example examples key-fn]
  (let [features (key-fn example)]
    (and (seq features)
         (some #(>= (jaccard-similarity features (key-fn %)) shared/duplication-threshold)
               (remove #{example} examples)))))

(defn- coverage-matrix-count
  [examples]
  (count
    (filter
      (fn [example]
        (and (coverage-matrix-candidate? example)
             (or (similar-to-any? example examples :setup-features)
                 (similar-to-any? example examples :arrange-features)
                 (and (similar-to-any? example examples :assert-features)
                      (similar-to-any? example examples :subject-symbols)))))
      examples)))

(defn- summarize-duplication
  [examples]
  (let [setup-duplication-score (similar-example-count examples :setup-features)
        assertion-duplication-score (similar-example-count examples :assert-features)
        fixture-duplication-score (similar-example-count examples :fixture-features)
        literal-duplication-score (similar-example-count examples :literal-features)
        arrange-duplication-score (similar-example-count examples :arrange-features)
        subject-repetition-score (similar-example-count examples :subject-symbols)]
    {:setup-duplication-score setup-duplication-score
     :assertion-duplication-score assertion-duplication-score
     :fixture-duplication-score fixture-duplication-score
     :literal-duplication-score literal-duplication-score
     :arrange-duplication-score arrange-duplication-score
     :repeated-setup-examples setup-duplication-score
     :repeated-fixture-examples fixture-duplication-score
     :repeated-literal-examples literal-duplication-score
     :repeated-arrange-examples arrange-duplication-score
     :subject-repetition-score subject-repetition-score
     :setup-shape-diversity (distinct-shape-count examples :setup-signatures)
     :assert-shape-diversity (distinct-shape-count examples :assert-signatures)
     :literal-shape-diversity (distinct-shape-count examples :literal-signatures)
     :arrange-shape-diversity (distinct-shape-count examples :arrange-signatures)
     :avg-setup-similarity (average-similarity examples :setup-features)
     :avg-assert-similarity (average-similarity examples :assert-features)
     :avg-fixture-similarity (average-similarity examples :fixture-features)
     :avg-literal-similarity (average-similarity examples :literal-features)
     :avg-arrange-similarity (average-similarity examples :arrange-features)
     :avg-subject-similarity (average-similarity examples :subject-symbols)
     :duplication-score (+ setup-duplication-score
                           assertion-duplication-score
                           fixture-duplication-score
                           literal-duplication-score
                           arrange-duplication-score)
     :harmful-duplication-score (+ setup-duplication-score
                                   assertion-duplication-score
                                   fixture-duplication-score
                                   arrange-duplication-score)}))

(defn summarize-examples
  [examples]
  (let [total (count examples)
        avg-scrap (if (pos? total)
                    (/ (reduce + (map :scrap examples)) total)
                    0.0)
        duplication (summarize-duplication examples)
        coverage-matrix-candidates (coverage-matrix-count examples)
        helper-hidden-example-count (count (filter #(pos? (:helper-hidden-lines %)) examples))
        zero-assertion-examples (count (filter #(zero? (:assertions %)) examples))
        table-driven-examples (count (filter :table-driven? examples))]
    (merge
      {:example-count total
       :avg-scrap avg-scrap
       :max-scrap (if (seq examples) (apply max (map :scrap examples)) 0)
       :branching-examples (count (filter #(and (pos? (:branches %))
                                                (not (:table-driven? %)))
                                          examples))
       :low-assertion-examples (count (filter #(<= (:assertions %) 1) examples))
       :with-redefs-examples (count (filter #(pos? (:with-redefs %)) examples))
       :zero-assertion-examples zero-assertion-examples
       :helper-hidden-example-count helper-hidden-example-count
       :table-driven-examples table-driven-examples
       :coverage-matrix-candidates coverage-matrix-candidates}
      duplication
      {:case-matrix-repetition coverage-matrix-candidates
       :coverage-matrix-candidates coverage-matrix-candidates
       :effective-duplication-score (max 0 (- (:harmful-duplication-score duplication)
                                              (quot coverage-matrix-candidates 2)))})))

(defn summarize-blocks
  [examples]
  (->> examples
       (group-by :describe-path)
       (remove (fn [[path _]] (empty? path)))
       (map (fn [[path block-examples]]
              {:path path
               :summary (summarize-examples block-examples)
               :worst-example (first (sort-by :scrap > block-examples))}))
       (sort-by (fn [{:keys [path]}]
                  [(count path) (str/join " / " path)]))
       vec))
