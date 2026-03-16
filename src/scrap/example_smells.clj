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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:57:23.858646-05:00", :module-hash "304074667", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "1894027595"} {:id "defn-/smell-rule", :kind "defn-", :line 3, :end-line 6, :hash "1456997993"} {:id "defn/smell-entries", :kind "defn", :line 8, :end-line 21, :hash "-633444422"}]}
;; clj-mutate-manifest-end
