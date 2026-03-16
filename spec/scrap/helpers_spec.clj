(ns scrap.helpers-spec
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [scrap.actionability :as actionability]
            [scrap.actionability-modes :as actionability-modes]
            [scrap.actionability-rules :as actionability-rules]
            [scrap.cli :as cli]
            [scrap.comparison :as comparison]
            [scrap.example-helpers :as example-helpers]
            [scrap.example-node :as example-node]
            [scrap.example-smells :as example-smells]
            [scrap.policy :as policy]
            [scrap.pressure-mode :as pressure-mode]
            [scrap.pressure-score :as pressure-score]
            [scrap.pressure-stability :as pressure-stability]
            [scrap.report :as report]
            [scrap.report-model :as report-model]
            [scrap.report-summary :as report-summary]
            [scrap.scan :as scan]
            [scrap.scan-tokenize :as scan-tokenize]
            [speclj.core :refer :all]))

(def rich-summary
  {:avg-scrap 14.0
   :max-scrap 24
   :example-count 6
   :low-assertion-examples 3
   :zero-assertion-examples 1
   :branching-examples 2
   :with-redefs-examples 1
   :duplication-score 12
   :harmful-duplication-score 5
   :effective-duplication-score 4
   :coverage-matrix-candidates 1
   :case-matrix-repetition 1
   :subject-repetition-score 2
   :helper-hidden-example-count 1
   :setup-duplication-score 2
   :assertion-duplication-score 1
   :fixture-duplication-score 1
   :literal-duplication-score 1
   :arrange-duplication-score 1
   :avg-setup-similarity 0.4
   :avg-assert-similarity 0.3
   :avg-arrange-similarity 0.2})

(def cli-compare-stub
  {:verdict :mixed
   :file-score-delta -1.0
   :avg-scrap-delta -1.0
   :max-scrap-delta -1
   :harmful-duplication-delta 0
   :case-matrix-delta 0
   :helper-hidden-delta 0})

(def report-guidance-result
  {:file-score 42.5
   :file-level "HIGH"
   :remediation-mode "LOCAL"
   :ai-actionability "AUTO_REFACTOR"
   :ai-actionability-message "Tighten the examples."
   :actions [{:label "HIGH" :text "Strengthen assertions first."}]
   :top-blocks [{:path ["alpha"]
                 :summary {:avg-scrap 18 :harmful-duplication-score 3}
                 :worst-example {:name "case 1" :scrap 22}}]
   :top-examples [{:describe-path ["alpha"] :name "case 1" :scrap 22 :smells ["multiple-phases"]}]})

(def report-comparison
  {:verdict :mixed
   :file-score-delta -1.0
   :avg-scrap-delta -1.0
   :max-scrap-delta -1
   :harmful-duplication-delta 0
   :case-matrix-delta 0
   :helper-hidden-delta 0})

(def report-example
  {:describe-path ["suite"]
   :name "slow case"
   :scrap 20
   :complexity 6
   :complexity-score 7.5
   :line-count 12
   :raw-line-count 14
   :assertions 1
   :branches 2
   :setup-depth 3
   :with-redefs 1
   :helper-calls 2
   :helper-hidden-lines 3
   :table-driven? false
   :smells ["multiple-phases"]})

(describe "actionability-modes"
  (it "classifies stable, matrix, local, split, and review-first summaries"
    (should= "LEAVE_ALONE"
             (:mode (actionability-modes/ai-actionability
                      {:example-count 1 :max-scrap 4 :effective-duplication-score 0}
                      [])))
    (should= "AUTO_TABLE_DRIVE"
             (:mode (actionability-modes/ai-actionability
                      {:example-count 4
                       :max-scrap 11
                       :low-assertion-examples 2
                       :coverage-matrix-candidates 4
                       :case-matrix-repetition 2
                       :effective-duplication-score 3}
                      [])))
    (should= "AUTO_REFACTOR"
             (:mode (actionability-modes/ai-actionability
                      {:example-count 5
                       :max-scrap 21
                       :low-assertion-examples 3
                       :effective-duplication-score 2}
                      [])))
    (should= "MANUAL_SPLIT"
             (:mode (actionability-modes/ai-actionability
                      {:example-count 12
                       :avg-scrap 10
                       :max-scrap 35
                       :effective-duplication-score 4}
                      [])))
    (should= "REVIEW_FIRST"
             (:mode (actionability-modes/ai-actionability
                      {:example-count 3 :max-scrap 14}
                      [])))))

(describe "actionability-rules"
  (it "emits all local recommendations when every rule is enabled"
    (let [actions (actionability-rules/local-action-rules
                    {:coverage-matrix-candidates 1
                     :max-scrap 21
                     :helper-hidden-example-count 1
                     :avg-scrap 13}
                    "LOCAL"
                    0.2
                    0.5
                    0.4
                    0.4
                    2)]
      (should= 8 (count actions))
      (should (some #(= "Convert repeated low-complexity examples into table-driven checks; treat this as coverage-matrix repetition, not harmful duplication." (:text %)) actions))
      (should (some #(= "LOW" (:label %)) actions))))

  (it "returns split guidance only for split mode"
    (should= nil (actionability-rules/split-rule "LOCAL"))
    (should= "HIGH" (:label (actionability-rules/split-rule "SPLIT")))))

(describe "actionability"
  (it "builds guidance output from summary, blocks, and examples"
    (let [report {:summary (assoc rich-summary :branching-examples 1)
                  :blocks [{:summary {:example-count 2 :avg-scrap 20 :max-scrap 24}
                            :path ["alpha"]
                            :worst-example {:name "first" :scrap 24}}
                           {:summary {:example-count 2 :avg-scrap 10 :max-scrap 11}
                            :path ["beta"]
                            :worst-example {:name "second" :scrap 11}}]
                  :examples [{:describe-path ["alpha"] :name "first" :scrap 24 :smells ["multiple-phases"]}
                             {:describe-path ["beta"] :name "second" :scrap 11 :smells []}]}
          result (actionability/guidance report)]
      (should= "HIGH" (:file-level result))
      (should= "AUTO_REFACTOR" (:ai-actionability result))
      (should= 2 (count (:top-blocks result)))
      (should= 2 (count (:top-examples result)))
      (should= 4 (count (:actions result))))))

(describe "policy"
  (it "exposes validated split policy maps with explicit domains"
    (should= 3 (:top-block-count policy/report-policy))
    (should= 0.5 (:threshold policy/duplication-policy))
    (should= 25.0 (:cap policy/complexity-policy))
    (should= 35 (get-in policy/pressure-policy [:levels :high]))
    (should= 4 (:max-actions policy/actionability-policy))))

(describe "pressure"
  (it "computes ratios and stable summaries"
    (should= 0.0 (pressure-stability/ratio 3 0))
    (should= 1/2 (pressure-stability/ratio 1 2))
    (should (pressure-stability/stable-summary?
              {:example-count 1
               :max-scrap 5
               :effective-duplication-score 0
               :helper-hidden-example-count 0}))
    (should-not (pressure-stability/stable-summary?
                  {:example-count 3
                   :max-scrap 13
                   :effective-duplication-score 4
                   :low-assertion-examples 2})))

  (it "scores pressure and classifies levels across size buckets"
    (let [scores [(pressure-score/refactor-pressure-score {:example-count 1 :avg-scrap 10 :max-scrap 10})
                  (pressure-score/refactor-pressure-score {:example-count 2 :avg-scrap 10 :max-scrap 10})
                  (pressure-score/refactor-pressure-score {:example-count 4 :avg-scrap 10 :max-scrap 10})
                  (pressure-score/refactor-pressure-score {:example-count 5 :avg-scrap 10 :max-scrap 10})]]
      (should (< (first scores) (second scores) (nth scores 2) (nth scores 3)))
      (should= "STABLE" (pressure-score/pressure-level {:example-count 1 :max-scrap 5 :effective-duplication-score 0}))
      (should= "LOW" (pressure-score/pressure-level {:example-count 3 :avg-scrap 2 :max-scrap 13}))
      (should= "MEDIUM" (pressure-score/pressure-level {:example-count 3 :avg-scrap 15 :max-scrap 18}))
      (should= "HIGH" (pressure-score/pressure-level {:example-count 4 :avg-scrap 30 :max-scrap 30}))
      (should= "CRITICAL" (pressure-score/pressure-level {:example-count 6 :avg-scrap 30 :max-scrap 40}))))

  (it "selects stable, split, and local remediation modes"
    (should= "STABLE"
             (pressure-mode/remediation-mode
               {:example-count 1 :max-scrap 4 :effective-duplication-score 0}
               []))
    (should= "SPLIT"
             (pressure-mode/remediation-mode
               {:example-count 12
                :avg-scrap 10
                :max-scrap 35
                :effective-duplication-score 4}
               []))
    (should= "LOCAL"
             (pressure-mode/remediation-mode
               {:example-count 6 :avg-scrap 4 :max-scrap 13}
               [])))

  (it "detects split triggers and block-driven split pressure"
    (should (#'pressure-mode/split-pressure? {:avg-scrap 10}))
    (should (#'pressure-mode/split-pressure? {:effective-duplication-score 20}))
    (should (#'pressure-mode/split-pressure? {:subject-repetition-score 12}))
    (should (#'pressure-mode/split-pressure? {:helper-hidden-example-count 1}))
    (should= "SPLIT"
             (pressure-mode/remediation-mode
               {:example-count 12
                :avg-scrap 10
                :max-scrap 20
                :effective-duplication-score 0}
               [{:summary {:example-count 3 :avg-scrap 30 :max-scrap 30}}
                {:summary {:example-count 3 :avg-scrap 30 :max-scrap 30}}]))))

(describe "comparison"
  (it "reports improved, worse, unchanged, mixed, and missing baselines"
    (let [baseline {:reports [{:path "spec/a_spec.clj"
                               :content-hash "a1"
                               :summary {:example-count 4 :avg-scrap 20 :max-scrap 20 :harmful-duplication-score 5}}
                              {:path "spec/b_spec.clj"
                               :content-hash "b1"
                               :summary {:example-count 4 :avg-scrap 10 :max-scrap 10 :harmful-duplication-score 1}}
                              {:path "spec/c_spec.clj"
                               :content-hash "c1"
                               :summary {:example-count 4 :avg-scrap 10 :max-scrap 10 :harmful-duplication-score 1}}
                              {:path "spec/d_spec.clj"
                               :content-hash "d1"
                               :summary {:example-count 4 :avg-scrap 10 :max-scrap 12 :harmful-duplication-score 1}}]}
          reports [{:path "spec/a_spec.clj" :content-hash "a2" :summary {:example-count 4 :avg-scrap 5 :max-scrap 5 :harmful-duplication-score 3}}
                   {:path "spec/b_spec.clj" :content-hash "b2" :summary {:example-count 4 :avg-scrap 12 :max-scrap 12 :harmful-duplication-score 2}}
                   {:path "spec/c_spec.clj" :content-hash "c2" :summary {:example-count 4 :avg-scrap 10 :max-scrap 10 :harmful-duplication-score 1}}
                   {:path "spec/d_spec.clj" :content-hash "d2" :summary {:example-count 4 :avg-scrap 9 :max-scrap 12 :harmful-duplication-score 1}}
                   {:path "spec/e_spec.clj" :content-hash "e2" :summary {:example-count 4 :avg-scrap 8 :max-scrap 8 :harmful-duplication-score 1}}]
          compared (comparison/compare-reports baseline reports)]
      (should= :improved (get-in (nth compared 0) [:comparison :verdict]))
      (should= :worse (get-in (nth compared 1) [:comparison :verdict]))
      (should= :unchanged (get-in (nth compared 2) [:comparison :verdict]))
      (should= :mixed (get-in (nth compared 3) [:comparison :verdict]))
      (should= nil (:comparison (nth compared 4)))))

  (it "recognizes helper regressions and worsening comparisons directly"
    (should (#'comparison/helper-regression?
              {:helper-hidden-delta 1
               :harmful-duplication-delta 0
               :case-matrix-delta 0}))
    (should (#'comparison/worsening-comparison?
              {:harmful-duplication-delta 0
               :max-scrap-delta 1
               :file-score-delta 0}))
    (should= :worse
             (#'comparison/verdict
               {:file-score-delta 0
                :harmful-duplication-delta 0
                :max-scrap-delta 0
                :helper-hidden-delta 1
                :case-matrix-delta 0})))

  (it "treats helper-hidden growth as a worse public comparison when other deltas stay flat"
    (let [baseline {:reports [{:path "spec/helper_spec.clj"
                               :content-hash "h1"
                               :summary {:example-count 4
                                         :avg-scrap 10
                                         :max-scrap 10
                                         :harmful-duplication-score 1
                                         :case-matrix-repetition 0
                                         :helper-hidden-example-count 0}}]}
          reports [{:path "spec/helper_spec.clj"
                    :content-hash "h2"
                    :summary {:example-count 4
                              :avg-scrap 10
                              :max-scrap 10
                              :harmful-duplication-score 1
                              :case-matrix-repetition 0
                              :helper-hidden-example-count 1}}]
          comparison (:comparison (first (comparison/compare-reports baseline reports)))]
      (should= 1 (:helper-hidden-delta comparison))
      (should= 0 (:harmful-duplication-delta comparison))
      (should= 0 (:case-matrix-delta comparison))
      (should (< 0 (:file-score-delta comparison)))
      (should= :worse (:verdict comparison)))))

(describe "cli"
  (it "parses args and sanitizes baseline output paths"
    (should= {:help true
              :verbose true
              :json true
              :write-baseline true
              :compare "baseline.json"
              :paths ["spec/a" "spec/b"]}
             (#'cli/parse-args ["--help" "--verbose" "--json" "--write-baseline" "--compare" "baseline.json" "spec/a" "spec/b"]))
    (should= "target/scrap/spec_alpha_beta.json"
             (#'cli/baseline-output-path ["spec/alpha" "beta"])))

  (it "returns help text without analysis"
    (let [result (cli/run-cli ["--help"])]
      (should= 0 (:exit-code result))
      (should-contain "Usage: clj -M:scrap" (:stdout result))))

  (it "renders json output for analyzed files"
    (with-redefs [scrap.analyze/collect-spec-files (fn [_] ["spec/custom"])
                  scrap.analyze/analyze-file (fn [path] {:path path :content-hash "new" :summary {:example-count 1}})
                  scrap.analyze/baseline-document (fn [paths reports] {:paths paths :reports reports})
                  scrap.guidance/compare-reports (fn [_ reports] (mapv #(assoc % :comparison cli-compare-stub) reports))
                  scrap.report/render-json (fn [_ reports] (json/write-str {:count (count reports)}))
                  scrap.cli/read-json-file (fn [_] {:reports [{:path "spec/a_spec.clj" :content-hash "old" :summary {:example-count 1}}]})
                  scrap.cli/write-baseline! (fn [path _] path)]
      (let [result (cli/run-cli ["spec/custom" "--json" "--write-baseline" "--compare" "baseline.json"])]
          (should= 0 (:exit-code result))
          (should-contain "\"count\":1" (:stdout result)))))

  (it "writes a baseline file when requested"
    (let [written-path (atom nil)]
      (with-redefs [scrap.analyze/collect-spec-files (fn [_] ["spec/custom"])
                    scrap.analyze/analyze-file (fn [path] {:path path :content-hash "new" :summary {:example-count 1}})
                    scrap.analyze/baseline-document (fn [paths reports] {:paths paths :reports reports})
                    scrap.guidance/compare-reports (fn [_ reports] (mapv #(assoc % :comparison cli-compare-stub) reports))
                    scrap.report/render-json (fn [_ reports] (json/write-str {:count (count reports)}))
                    scrap.cli/read-json-file (fn [_] {:reports [{:path "spec/a_spec.clj" :content-hash "old" :summary {:example-count 1}}]})
                    scrap.cli/write-baseline! (fn [path _]
                                                (reset! written-path path)
                                                path)]
        (let [result (cli/run-cli ["spec/custom" "--json" "--write-baseline" "--compare" "baseline.json"])]
          (should= 0 (:exit-code result))
          (should-contain "Baseline written: target/scrap/spec_custom.json" (:stdout result))
          (should= "target/scrap/spec_custom.json" @written-path))))))

(describe "example-smells"
  (it "returns the expected smell entries for strong and weak examples"
    (should= []
             (example-smells/smell-entries {:assertions 2 :with-redefs 0 :temp-resources 0 :large-literals 0 :helper-hidden-lines 0}
                                           8
                                           1
                                           true
                                           true))
    (let [entries (example-smells/smell-entries {:assertions 0 :with-redefs 4 :temp-resources 1 :large-literals 1 :helper-hidden-lines 9}
                                                24
                                                2
                                                false
                                                false)]
      (should= ["no-assertions"
                "multiple-phases"
                "high-mocking"
                "large-example"
                "temp-resource-work"
                "literal-heavy-setup"
                "helper-hidden-complexity"]
               (mapv :label entries)))))

(describe "example-node"
  (it "reuses cached helper metrics and classifies top-level forms"
    (let [helper-defs {'helper ['(should= 1 1)]}
          helper-cache (atom {})
          helper-context {:helper-defs helper-defs
                          :helper-cache helper-cache}
          first-metrics (example-node/helper-expanded-metrics helper-context 'helper #{} 0)
          second-metrics (example-node/helper-expanded-metrics helper-context 'helper #{} 0)]
      (should= first-metrics second-metrics)
      (should= 1 (count @helper-cache))
      (should= :setup (example-node/top-level-phase '(before (reset! state 1)) helper-context))
      (should= :action (example-node/top-level-phase '(println "x") helper-context))
      (should= 1 (example-node/assertion-clusters ['(should= 1 1) '(should= 2 2)] helper-context))))

  (it "marks table-driven forms under branch heads when a large case table is present"
    (let [expr '(doseq [[input expected] [[1 1] [2 2]]]
                  (should= expected input))
          metrics (example-node/analyze-node expr {:helper-defs {} :helper-cache (atom {})} 0 #{})]
      (should (:table-driven? metrics))
      (should= 1 (:table-branches metrics))))

  (it "tracks helper-hidden metrics for helper calls"
    (let [helper-context (example-helpers/helper-context
                           ['(defn helper []
                               (should= 1 1))
                            '(helper)])
          metrics (example-node/analyze-node '(helper) helper-context 0 #{})]
      (should= 1 (:helper-calls metrics))
      (should (< 0 (:helper-hidden-lines metrics)))))

  (it "returns zero hidden lines when a helper body is missing"
    (let [helper-context {:helper-defs {}
                          :helper-cache (atom {})}
          metrics (example-node/helper-expanded-metrics helper-context 'missing-helper #{} 0)]
      (should= {:helper-hidden-lines 0} metrics))))

(describe "report"
  (it "validates analysis and guidance report models"
    (should= "spec/a_spec.clj"
             (:path (report-model/analysis-report
                      {:path "spec/a_spec.clj"
                       :content-hash "abc123"
                       :structure-errors []
                       :parse-error nil
                       :examples []
                       :blocks []})))
    (should= "HIGH"
             (:file-level (report-model/guidance-report
                            {:file-score 10.0
                             :file-level "HIGH"
                             :remediation-mode "LOCAL"
                             :ai-actionability "AUTO_REFACTOR"
                             :ai-actionability-message "Tighten the examples."
                             :actions []
                             :top-blocks []
                             :top-examples []})))
    (should-throw clojure.lang.ExceptionInfo
                  (report-model/analysis-report
                    {:path nil
                     :content-hash "abc123"
                     :structure-errors []
                     :parse-error nil
                     :examples []
                     :blocks []})))

  (it "renders comparison sections in guidance reports"
    (with-redefs [scrap.guidance/guidance (fn [_]
                                            report-guidance-result)
                scrap.guidance/pressure-level (fn [_] "HIGH")
                scrap.guidance/ratio (fn [n d] (if (pos? d) (/ n d) 0.0))]
      (let [output (report/render-report
                     [{:path "spec/a_spec.clj"
                       :summary rich-summary
                       :comparison {:verdict :worse
                                    :file-score-delta 2.0
                                    :avg-scrap-delta 1.0
                                    :max-scrap-delta 2
                                    :harmful-duplication-delta 1
                                    :case-matrix-delta 0
                                    :helper-hidden-delta 0}}]
                     false)]
        (should-contain "comparison:" output)
        (should-contain "recommendation: Refactor appears negative" output)
        (should-contain "why:" output))))

  (it "renders location sections in guidance reports"
    (with-redefs [scrap.guidance/guidance (fn [_]
                                            report-guidance-result)
                scrap.guidance/pressure-level (fn [_] "HIGH")
                scrap.guidance/ratio (fn [n d] (if (pos? d) (/ n d) 0.0))]
      (let [output (report/render-report
                     [{:path "spec/a_spec.clj"
                       :summary rich-summary
                       :comparison {:verdict :worse
                                    :file-score-delta 2.0
                                    :avg-scrap-delta 1.0
                                    :max-scrap-delta 2
                                    :harmful-duplication-delta 1
                                    :case-matrix-delta 0
                                    :helper-hidden-delta 0}}]
                     false)]
        (should-contain "where:" output)
        (should-contain "worst-examples:" output))))

  (it "renders action sections in guidance reports"
    (with-redefs [scrap.guidance/guidance (fn [_]
                                            report-guidance-result)
                scrap.guidance/pressure-level (fn [_] "HIGH")
                scrap.guidance/ratio (fn [n d] (if (pos? d) (/ n d) 0.0))]
      (let [output (report/render-report
                     [{:path "spec/a_spec.clj"
                       :summary rich-summary
                       :comparison {:verdict :worse
                                    :file-score-delta 2.0
                                    :avg-scrap-delta 1.0
                                    :max-scrap-delta 2
                                    :harmful-duplication-delta 1
                                    :case-matrix-delta 0
                                    :helper-hidden-delta 0}}]
                     false)]
        (should-contain "how:" output)
        (should-contain "HIGH: Strengthen assertions first." output))))

  (it "renders summary helper sections"
    (let [why-output (report-summary/guidance-why-section rich-summary)
          verbose-output (report-summary/verbose-summary-section rich-summary)]
      (should-contain "low-assertion-ratio" why-output)
      (should-contain "branching-examples: 2/6" verbose-output)
      (should-contain "avg-arrange-similarity: 0.20" verbose-output)))

  (it "renders verbose report diagnostics"
    (let [output (report/render-report
                   [{:path "spec/b_spec.clj"
                     :comparison report-comparison
                     :structure-errors ["bad nesting"]
                     :parse-error "parse failed"
                     :summary rich-summary
                     :blocks [{:path ["suite"]
                               :summary rich-summary
                               :worst-example {:name "slow case" :scrap 20}}]
                     :examples [report-example]}]
                   true)]
      (should-contain "structure-errors:" output)
      (should-contain "parse-error: parse failed" output)
      (should-contain "blocks:" output)
      (should-contain "SCRAP: 20.0" output)))

  (it "renders json payloads"
    (let [json-output (report/render-json ["spec/b_spec.clj"] [{:path "spec/b_spec.clj"}])]
      (should-contain "\"baseline-version\"" json-output))))

(describe "scan"
  (it "tokenizes strings, comments, regexes, and escapes correctly"
    (should= {:mode :string :line 1 :escape true :skip false}
             (scan-tokenize/process-char {:mode :string :line 1 :escape false :skip false} \\ nil))
    (should= {:mode :normal :line 1 :escape false :skip false}
             (scan-tokenize/process-char {:mode :string :line 1 :escape false :skip false} \" nil))
    (should= {:mode :normal :line 3 :escape false :skip false}
             (scan-tokenize/process-char {:mode :comment :line 2 :escape false :skip false} \newline nil))
    (should= {:mode :regex :line 1 :escape false :skip true :depth 0}
             (scan-tokenize/process-char {:mode :normal :line 1 :escape false :skip false :depth 0} \# \"))
    (should= "describe"
             (scan-tokenize/extract-token (vec "(describe \"x\")") 0)))

  (it "scans nested forms while ignoring comments, strings, and regexes"
    (let [source (str "; (it \"ignored\")\n"
                      "(describe \"x\"\n"
                      "  #\"(it still ignored)\"\n"
                      "  (context \"ctx\"\n"
                      "    (it \"works\"\n"
                      "      \"(it in string)\"\n"
                      "      (should= 1 1))))\n")
          errors (scan/scan-structure source)]
      (should= [] errors)))

  (it "reports illegal nesting inside it blocks"
    (let [errors (scan/scan-structure
                   "(describe \"x\" (it \"outer\" (context \"bad\" (should= 1 1))))")]
      (should (some #(str/includes? % "(context) inside (it)") errors)))))
