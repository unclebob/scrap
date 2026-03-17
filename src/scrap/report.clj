(ns scrap.report
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [scrap.guidance :as guidance]
            [scrap.report-model :as report-model]
            [scrap.report-summary :as report-summary]))

(defn- round1
  [n]
  (/ (Math/round (* 10.0 (double n))) 10.0))

(defn- format-smells
  [smells]
  (if (seq smells)
    (str/join ", " smells)
    "none"))

(defn- format-line-range
  [{:keys [line end-line]}]
  (if (= line end-line)
    (str "line " line)
    (str "lines " line "-" end-line)))

(defn- render-recommended-extraction
  [{:keys [it-names examples net-benefit shared-forms variable-points]}]
  (str "    "
       (str/join ", " it-names)
       " -> "
       (str/join "; " (map format-line-range examples))
       ", benefit "
       (format "%.1f" (double (round1 net-benefit)))
       ", F " shared-forms
       ", V " variable-points))

(defn- recommended-extractions-section
  [summary]
  (let [recommendations (:recommended-extractions summary)]
    (when (seq recommendations)
      (str "  recommended-extractions:\n"
           (str/join "\n" (map render-recommended-extraction (take 5 recommendations)))
           "\n"))))

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
         (report-summary/guidance-why-section summary)
         (render-comparison report)
         (guidance-where-section top-blocks)
         (guidance-worst-section top-examples)
         (recommended-extractions-section summary)
         (guidance-how-section actions))))

(defn- render-verbose-block
  [{:keys [path summary worst-example]}]
  (str "    "
       (str/join " / " path) "\n"
       "      examples: " (:example-count summary) "\n"
       "      avg-scrap: " (format "%.1f" (double (or (:avg-scrap summary) 0.0))) "\n"
       "      max-scrap: " (format "%.1f" (double (round1 (:max-scrap summary)))) "\n"
       "      recommended-extraction-count: " (or (:recommended-extraction-count summary) 0) "\n"
       "      extraction-pressure-score: " (format "%.1f" (double (or (:extraction-pressure-score summary) 0.0))) "\n"
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
       (report-summary/verbose-summary-section summary)
       (recommended-extractions-section summary)
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
  (json/write-str (report-model/baseline-document paths reports)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T14:53:12.400341-05:00", :module-hash "-322217995", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "-643333173"} {:id "defn-/round1", :kind "defn-", :line 8, :end-line 10, :hash "991386479"} {:id "defn-/format-smells", :kind "defn-", :line 12, :end-line 16, :hash "-1571160262"} {:id "defn-/render-comparison", :kind "defn-", :line 18, :end-line 30, :hash "1317457391"} {:id "defn-/render-top-block", :kind "defn-", :line 32, :end-line 44, :hash "-2093400885"} {:id "defn-/guidance-where-section", :kind "defn-", :line 46, :end-line 51, :hash "814006351"} {:id "defn-/render-top-example", :kind "defn-", :line 53, :end-line 59, :hash "-604010348"} {:id "defn-/guidance-worst-section", :kind "defn-", :line 61, :end-line 66, :hash "-869739732"} {:id "defn-/guidance-how-section", :kind "defn-", :line 68, :end-line 73, :hash "-2046202976"} {:id "defn-/render-guidance-report", :kind "defn-", :line 75, :end-line 96, :hash "-1700682112"} {:id "defn-/render-verbose-block", :kind "defn-", :line 98, :end-line 108, :hash "-1870139994"} {:id "defn-/verbose-blocks-section", :kind "defn-", :line 110, :end-line 114, :hash "-1125870309"} {:id "defn-/render-verbose-example", :kind "defn-", :line 116, :end-line 132, :hash "276516332"} {:id "defn-/verbose-examples-section", :kind "defn-", :line 134, :end-line 138, :hash "-2016491541"} {:id "defn-/render-file-report", :kind "defn-", :line 140, :end-line 152, :hash "-1793687341"} {:id "defn/render-report", :kind "defn", :line 154, :end-line 173, :hash "331681477"} {:id "defn/render-json", :kind "defn", :line 175, :end-line 177, :hash "-1819782255"}]}
;; clj-mutate-manifest-end
