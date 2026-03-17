(ns scrap.extraction-pressure
  (:require [clojure.set :as set]
            [scrap.policy :as policy]))

(defn- duplication-threshold
  []
  (:threshold policy/duplication))

(defn- jaccard-similarity
  [a b]
  (let [a (or a #{})
        b (or b #{})
        union-size (count (set/union a b))]
    (if (zero? union-size)
      0.0
      (/ (count (set/intersection a b))
         union-size))))

(defn- harmful-features
  [example]
  (apply set/union #{}
         [(or (:setup-features example) #{})
          (or (:assert-features example) #{})
          (or (:fixture-features example) #{})
          (or (:arrange-features example) #{})]))

(defn- edge-map
  [examples]
  (reduce
    (fn [adjacency [left-index left right-index right]]
      (if (>= (jaccard-similarity (harmful-features left) (harmful-features right))
              (duplication-threshold))
        (-> adjacency
            (update left-index (fnil conj #{}) right-index)
            (update right-index (fnil conj #{}) left-index))
        adjacency))
    {}
    (for [[left-index left] (map-indexed vector examples)
          [right-index right] (drop (inc left-index) (map-indexed vector examples))]
      [left-index left right-index right])))

(defn- connected-components
  [examples]
  (let [adjacency (edge-map examples)
        indexes (range (count examples))]
    (loop [remaining (set indexes)
           components []]
      (if-let [start (first remaining)]
        (let [component (loop [stack [start]
                               seen #{}]
                          (if-let [idx (peek stack)]
                            (if (seen idx)
                              (recur (pop stack) seen)
                              (recur (into (pop stack) (get adjacency idx #{}))
                                     (conj seen idx)))
                            seen))]
          (recur (apply disj remaining component)
                 (if (> (count component) 1)
                   (conj components (mapv examples (sort component)))
                   components)))
        components))))

(defn- coverage-matrix-candidate?
  [example]
  (let [{:keys [matrix-max-scrap
                matrix-max-lines
                matrix-max-assertions
                matrix-max-branches
                matrix-max-setup-depth
                matrix-max-with-redefs
                matrix-max-temp-resources
                matrix-max-helper-hidden-lines
                matrix-max-subject-symbols]} policy/duplication]
    (and (<= (:scrap example) matrix-max-scrap)
         (<= (:line-count example) matrix-max-lines)
         (<= (:assertions example) matrix-max-assertions)
         (<= (:branches example) matrix-max-branches)
         (<= (:setup-depth example) matrix-max-setup-depth)
         (<= (:with-redefs example) matrix-max-with-redefs)
         (<= (or (:temp-resources example) 0) matrix-max-temp-resources)
         (<= (or (:helper-hidden-lines example) 0) matrix-max-helper-hidden-lines)
         (or (:table-driven? example)
             (and (<= (count (:subject-symbols example)) matrix-max-subject-symbols)
                  (seq (harmful-features example)))))))

(defn- duplication-cost
  [shared-forms instance-count variable-points]
  (let [min-shared-forms 3
        max-variable-points 4]
    (cond
      (<= shared-forms min-shared-forms) 0.0
      (> variable-points max-variable-points) 0.0
      :else (/ (* (max 0 (- shared-forms min-shared-forms))
                  (Math/pow (max 0 (dec instance-count)) 1.5))
               (inc variable-points)))))

(defn- recommendation
  [cluster]
  (let [feature-sets (map harmful-features cluster)
        shared-features (apply set/intersection feature-sets)
        all-features (apply set/union feature-sets)
        shared-forms (count shared-features)
        variable-points (- (count all-features) shared-forms)
        instances (count cluster)
        duplication-before (duplication-cost shared-forms instances variable-points)
        duplication-after 0.0
        helper-cost (+ shared-forms variable-points)
        net-benefit (max 0.0 (- duplication-before duplication-after helper-cost))]
    {:it-names (mapv :name cluster)
     :examples (mapv (fn [{:keys [name line end-line describe-path]}]
                       {:name name
                        :describe-path describe-path
                        :line line
                        :end-line end-line})
                     cluster)
     :describe-path (->> cluster (map :describe-path) frequencies (apply max-key val) key)
     :line-start (apply min (map :line cluster))
     :line-end (apply max (map :end-line cluster))
     :instances instances
     :shared-forms shared-forms
     :variable-points variable-points
     :duplication-before duplication-before
     :duplication-after duplication-after
     :helper-cost helper-cost
     :net-benefit net-benefit}))

(defn summarize-extractions
  [examples]
  (let [recommendations (->> (connected-components examples)
                             (remove #(every? coverage-matrix-candidate? %))
                             (map recommendation)
                             (filter #(pos? (:net-benefit %)))
                             (sort-by (juxt :line-start (comp - :net-benefit)))
                             vec)]
    {:recommended-extraction-count (count recommendations)
     :extraction-pressure-score (reduce + (map :net-benefit recommendations))
     :recommended-extractions recommendations}))
