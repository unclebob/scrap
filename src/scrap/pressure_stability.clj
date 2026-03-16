(ns scrap.pressure-stability)

(defn ratio
  [n d]
  (if (pos? d) (/ n d) 0.0))

(defn- stable-small-summary?
  [summary example-count zero-assertion-ratio]
  (and (<= example-count 2)
       (<= (or (:max-scrap summary) 0) 10)
       (<= (or (:effective-duplication-score summary) 0) 1)
       (zero? (or (:helper-hidden-example-count summary) 0))
       (<= zero-assertion-ratio 0.0)))

(defn- stable-general-summary?
  [summary example-count zero-assertion-ratio low-assertion-ratio]
  (and (pos? example-count)
       (<= (or (:max-scrap summary) 0) 12)
       (<= (or (:effective-duplication-score summary) 0) 3)
       (<= zero-assertion-ratio 0.0)
       (<= low-assertion-ratio 0.35)))

(defn stable-summary?
  [summary]
  (let [example-count (or (:example-count summary) 0)
        zero-assertion-ratio (ratio (or (:zero-assertion-examples summary) 0) example-count)
        low-assertion-ratio (ratio (or (:low-assertion-examples summary) 0) example-count)]
    (or (stable-small-summary? summary example-count zero-assertion-ratio)
        (stable-general-summary? summary example-count zero-assertion-ratio low-assertion-ratio))))
