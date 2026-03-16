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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:06:35.303458-05:00", :module-hash "524298566", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-1079331527"} {:id "defn/ratio", :kind "defn", :line 3, :end-line 5, :hash "-1148533739"} {:id "defn-/stable-small-summary?", :kind "defn-", :line 7, :end-line 13, :hash "-523921607"} {:id "defn-/stable-general-summary?", :kind "defn-", :line 15, :end-line 21, :hash "420007535"} {:id "defn/stable-summary?", :kind "defn", :line 23, :end-line 29, :hash "-1057844211"}]}
;; clj-mutate-manifest-end
