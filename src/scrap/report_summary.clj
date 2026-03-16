(ns scrap.report-summary
  (:require [clojure.string :as str]
            [scrap.judgment :as judgment]))

(defn render-summary-lines
  [lines indent]
  (str/join "\n" (map (fn [[label value]] (str indent label ": " value)) lines)))

(defn- metric-line
  [summary {:keys [label key default formatter value-fn]}]
  (let [value (if value-fn
                (value-fn summary)
                (or (get summary key) default))]
    [label ((or formatter str) value)]))

(def why-line-specs
  [{:label "avg-scrap" :formatter #(format "%.1f" (double (or % 0.0))) :value-fn :avg-scrap}
   {:label "max-scrap" :key :max-scrap :default 0}
   {:label "harmful-duplication-score" :key :harmful-duplication-score :default 0}
   {:label "effective-duplication-score" :key :effective-duplication-score :default 0}
   {:label "coverage-matrix-candidates" :key :coverage-matrix-candidates :default 0}
   {:label "case-matrix-repetition" :key :case-matrix-repetition :default 0}
   {:label "subject-repetition-score" :key :subject-repetition-score :default 0}
   {:label "helper-hidden-example-count" :key :helper-hidden-example-count :default 0}])

(def verbose-line-specs
  [{:label "avg-scrap" :formatter #(format "%.1f" (double (or % 0.0))) :value-fn :avg-scrap}
   {:label "max-scrap" :formatter #(format "%.1f" (double %)) :value-fn :max-scrap}
   {:label "branching-examples" :value-fn #(str (:branching-examples %) "/" (:example-count %))}
   {:label "low-assertion-examples" :value-fn #(str (:low-assertion-examples %) "/" (:example-count %))}
   {:label "zero-assertion-examples" :value-fn #(str (:zero-assertion-examples %) "/" (:example-count %))}
   {:label "with-redefs-examples" :value-fn #(str (:with-redefs-examples %) "/" (:example-count %))}
   {:label "duplication-score" :key :duplication-score :default 0}
   {:label "harmful-duplication-score" :key :harmful-duplication-score :default 0}
   {:label "effective-duplication-score" :key :effective-duplication-score :default 0}
   {:label "coverage-matrix-candidates" :key :coverage-matrix-candidates :default 0}
   {:label "case-matrix-repetition" :key :case-matrix-repetition :default 0}
   {:label "subject-repetition-score" :key :subject-repetition-score :default 0}
   {:label "helper-hidden-example-count" :key :helper-hidden-example-count :default 0}
   {:label "setup-duplication-score" :key :setup-duplication-score :default 0}
   {:label "assertion-duplication-score" :key :assertion-duplication-score :default 0}
   {:label "fixture-duplication-score" :key :fixture-duplication-score :default 0}
   {:label "literal-duplication-score" :key :literal-duplication-score :default 0}
   {:label "arrange-duplication-score" :key :arrange-duplication-score :default 0}
   {:label "avg-setup-similarity" :formatter #(format "%.2f" (double (or % 0.0))) :value-fn :avg-setup-similarity}
   {:label "avg-assert-similarity" :formatter #(format "%.2f" (double (or % 0.0))) :value-fn :avg-assert-similarity}
   {:label "avg-arrange-similarity" :formatter #(format "%.2f" (double (or % 0.0))) :value-fn :avg-arrange-similarity}])

(defn- metric-lines
  [summary specs]
  (mapv #(metric-line summary %) specs))

(defn summary-ratio-lines
  [summary]
  (let [{:keys [low-assertion-ratio branching-ratio mocking-ratio]} (judgment/summary-ratios summary)]
    [["low-assertion-ratio" (format "%.2f" (double low-assertion-ratio))]
     ["branching-ratio" (format "%.2f" (double branching-ratio))]
     ["mocking-ratio" (format "%.2f" (double mocking-ratio))]]))

(defn guidance-why-section
  [summary]
  (let [base-lines (metric-lines summary why-line-specs)
        lines (concat base-lines (summary-ratio-lines summary))]
    (str "  why:\n"
         (render-summary-lines lines "    ")
         "\n")))

(defn verbose-summary-lines
  [summary]
  (metric-lines summary verbose-line-specs))

(defn verbose-summary-section
  [summary]
  (when summary
    (str (render-summary-lines (verbose-summary-lines summary) "  ")
         "\n")))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T14:34:21.017989-05:00", :module-hash "1083835785", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "2041167502"} {:id "defn/render-summary-lines", :kind "defn", :line 5, :end-line 7, :hash "-305372540"} {:id "defn-/metric-line", :kind "defn-", :line 9, :end-line 14, :hash "2112396391"} {:id "def/why-line-specs", :kind "def", :line 16, :end-line 24, :hash "-1269627151"} {:id "def/verbose-line-specs", :kind "def", :line 26, :end-line 47, :hash "273677513"} {:id "defn-/metric-lines", :kind "defn-", :line 49, :end-line 51, :hash "-1786652267"} {:id "defn/summary-ratio-lines", :kind "defn", :line 53, :end-line 58, :hash "-1956866886"} {:id "defn/guidance-why-section", :kind "defn", :line 60, :end-line 66, :hash "1114220517"} {:id "defn/verbose-summary-lines", :kind "defn", :line 68, :end-line 70, :hash "-1625042628"} {:id "defn/verbose-summary-section", :kind "defn", :line 72, :end-line 76, :hash "925045547"}]}
;; clj-mutate-manifest-end
