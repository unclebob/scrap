(ns scrap.example-smells)

(defn- smell-rule
  [enabled? label penalty]
  (when enabled?
    {:label label :penalty penalty}))

(defn smell-entries
  [metrics line-count phases table-driven? api-contract?]
  (->> [(smell-rule (zero? (:assertions metrics)) "no-assertions" 10)
        (smell-rule (and (= 1 (:assertions metrics)) (> line-count 10) (not table-driven?) (not api-contract?))
                    "low-assertion-density"
                    6)
        (smell-rule (> phases 1) "multiple-phases" 5)
        (smell-rule (> (:with-redefs metrics) 3) "high-mocking" 4)
        (smell-rule (and (> line-count 20) (not api-contract?)) "large-example" 4)
        (smell-rule (pos? (:temp-resources metrics)) "temp-resource-work" 3)
        (smell-rule (pos? (:large-literals metrics)) "literal-heavy-setup" 3)
        (smell-rule (> (:helper-hidden-lines metrics) 8) "helper-hidden-complexity" 4)]
       (remove nil?)
       vec))
