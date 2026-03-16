(ns scrap.report
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [scrap.guidance :as guidance]
            [scrap.shared :as shared]))

(defn- round1
  [n]
  (/ (Math/round (* 10.0 (double n))) 10.0))

(defn- format-smells
  [smells]
  (if (seq smells)
    (str/join ", " smells)
    "none"))

(defn- render-comparison
  [{:keys [comparison]}]
  (when comparison
    (str "  comparison:\n"
         "    verdict: " (name (:verdict comparison)) "\n"
         "    file-score-delta: " (format "%.1f" (double (:file-score-delta comparison))) "\n"
         "    avg-scrap-delta: " (format "%.1f" (double (:avg-scrap-delta comparison))) "\n"
         "    max-scrap-delta: " (:max-scrap-delta comparison) "\n"
         "    harmful-duplication-delta: " (:harmful-duplication-delta comparison) "\n"
         "    case-matrix-delta: " (:case-matrix-delta comparison) "\n"
         "    helper-hidden-delta: " (:helper-hidden-delta comparison) "\n"
         (when (= :worse (:verdict comparison))
           "    recommendation: Refactor appears negative; consider reverting or simplifying helper extraction.\n"))))

(defn- summary-ratio-lines
  [summary]
  [["low-assertion-ratio" (format "%.2f" (double (guidance/ratio (or (:low-assertion-examples summary) 0)
                                                                (or (:example-count summary) 0))))]
   ["branching-ratio" (format "%.2f" (double (guidance/ratio (or (:branching-examples summary) 0)
                                                            (or (:example-count summary) 0))))]
   ["mocking-ratio" (format "%.2f" (double (guidance/ratio (or (:with-redefs-examples summary) 0)
                                                          (or (:example-count summary) 0))))]])

(defn- render-summary-lines
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
   {:label "max-scrap" :formatter #(format "%.1f" (double (round1 %))) :value-fn #(or (:max-scrap %) 0)}
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

(defn- guidance-why-section
  [summary]
  (let [base-lines (metric-lines summary why-line-specs)
        lines (concat base-lines (summary-ratio-lines summary))]
    (str "  why:\n"
         (render-summary-lines lines "    ")
         "\n")))

(defn- verbose-summary-lines
  [summary]
  (metric-lines summary verbose-line-specs))

(defn- render-top-block
  [{:keys [path summary worst-example]}]
  (str "    "
       (str/join " / " path)
       " -> "
       (guidance/pressure-level summary)
       ", avg-scrap "
       (format "%.1f" (double (round1 (or (:avg-scrap summary) 0.0))))
       ", harmful duplication "
       (or (:harmful-duplication-score summary) 0)
       ", worst "
       (:name worst-example)
       " (SCRAP " (format "%.1f" (double (round1 (:scrap worst-example)))) ")"))

(defn- guidance-where-section
  [top-blocks]
  (when (seq top-blocks)
    (str "  where:\n"
         (str/join "\n" (map render-top-block top-blocks))
         "\n")))

(defn- render-top-example
  [example]
  (str "    "
       (str/join " / " (conj (:describe-path example) (:name example)))
       " -> SCRAP " (format "%.1f" (double (round1 (:scrap example))))
       (when (seq (:smells example))
         (str " [" (format-smells (:smells example)) "]"))))

(defn- guidance-worst-section
  [top-examples]
  (when (seq top-examples)
    (str "  worst-examples:\n"
         (str/join "\n" (map render-top-example top-examples))
         "\n")))

(defn- guidance-how-section
  [actions]
  (when (seq actions)
    (str "  how:\n"
         (str/join "\n" (map #(str "    " (:label %) ": " (:text %)) actions))
         "\n")))

(defn- render-guidance-report
  [{:keys [path] :as report}]
  (let [{:keys [file-score
                file-level
                remediation-mode
                ai-actionability
                ai-actionability-message
                actions
                top-blocks
                top-examples]} (guidance/guidance report)
        summary (:summary report)
        header (str "  remediation-mode: " remediation-mode "\n"
                    "  ai-actionability: " ai-actionability "\n"
                    "  ai-guidance: " ai-actionability-message "\n")]
    (str path "\n"
         "  refactor-pressure: " file-level " (" (format "%.1f" (double file-score)) ")\n"
         header
         (guidance-why-section summary)
         (render-comparison report)
         (guidance-where-section top-blocks)
         (guidance-worst-section top-examples)
         (guidance-how-section actions))))

(defn- verbose-summary-section
  [summary]
  (when summary
    (str (render-summary-lines (verbose-summary-lines summary) "  ")
         "\n")))

(defn- render-verbose-block
  [{:keys [path summary worst-example]}]
  (str "    "
       (str/join " / " path) "\n"
       "      examples: " (:example-count summary) "\n"
       "      avg-scrap: " (format "%.1f" (double (or (:avg-scrap summary) 0.0))) "\n"
       "      max-scrap: " (format "%.1f" (double (round1 (:max-scrap summary)))) "\n"
       "      harmful-duplication-score: " (or (:harmful-duplication-score summary) 0) "\n"
       "      coverage-matrix-candidates: " (or (:coverage-matrix-candidates summary) 0) "\n"
       "      case-matrix-repetition: " (or (:case-matrix-repetition summary) 0) "\n"
       "      worst-example: " (:name worst-example) " (SCRAP " (format "%.1f" (double (round1 (:scrap worst-example)))) ")"))

(defn- verbose-blocks-section
  [blocks]
  (when (seq blocks)
    (str "\n  blocks:\n"
         (str/join "\n" (map render-verbose-block blocks)))))

(defn- render-verbose-example
  [example]
  (str "    "
       (str/join " / " (conj (:describe-path example) (:name example))) "\n"
       "      SCRAP: " (format "%.1f" (double (round1 (:scrap example)))) "\n"
       "      complexity: " (or (:complexity example) 0) "\n"
       "      complexity-score: " (format "%.1f" (double (round1 (or (:complexity-score example) 0.0)))) "\n"
       "      lines: " (:line-count example) "\n"
       "      raw-lines: " (or (:raw-line-count example) 0) "\n"
       "      assertions: " (:assertions example) "\n"
       "      branches: " (:branches example) "\n"
       "      setup-depth: " (:setup-depth example) "\n"
       "      redefs: " (:with-redefs example) "\n"
       "      helper-calls: " (:helper-calls example) "\n"
       "      helper-hidden-lines: " (or (:helper-hidden-lines example) 0) "\n"
       "      table-driven: " (if (:table-driven? example) "yes" "no") "\n"
       "      smells: " (format-smells (:smells example))))

(defn- verbose-examples-section
  [examples]
  (when (seq examples)
    (str "\n"
         (str/join "\n" (map render-verbose-example (take 5 (sort-by :scrap > examples)))))))

(defn- render-file-report
  [{:keys [path structure-errors parse-error examples summary blocks] :as report}]
  (str path "\n"
       (when (seq structure-errors)
         (str "  structure-errors:\n"
              (str/join "\n" (map #(str "    " %) structure-errors))
              "\n"))
       (when parse-error
         (str "  parse-error: " parse-error "\n"))
       (render-comparison report)
       (verbose-summary-section summary)
       (verbose-blocks-section blocks)
       (verbose-examples-section examples)))

(defn render-report
  [file-reports verbose?]
  (let [worst (->> file-reports
                   (mapcat (fn [{:keys [path examples]}]
                             (map #(assoc % :file path) examples)))
                   (sort-by :scrap >)
                   (take 10))]
    (str "=== SCRAP Report ===\n\n"
         (str/join "\n\n" (map (if verbose? render-file-report render-guidance-report) file-reports))
         (when (seq worst)
           (str "\n\nWorst Examples:\n"
                (str/join
                  "\n"
                  (map-indexed
                    (fn [idx example]
                      (str "  " (inc idx) ". "
                           (:file example) " :: "
                           (str/join " / " (conj (:describe-path example) (:name example)))
                           "  SCRAP " (format "%.1f" (double (round1 (:scrap example))))))
                    worst)))))))

(defn render-json
  [paths reports]
  (json/write-str
    {:baseline-version shared/baseline-version
     :paths (vec paths)
     :reports reports}))
