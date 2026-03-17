(ns scrap.core-spec
  (:require [scrap.core :as scrap]
            [speclj.core :refer :all]))

(def complex-workflow-source
  (str "(describe \"workflow\"\n"
       "  (defn helper [] 42)\n"
       "  (before (helper))\n"
       "  (it \"does a lot\"\n"
       "    (with-redefs [foo inc] (helper))\n"
       "    (with-redefs [bar dec] (helper))\n"
       "    (with-redefs [baz identity] (helper))\n"
       "    (with-redefs [qux str]\n"
       "      (let [data {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}\n"
       "            result (if true (helper) 0)]\n"
       "        result))\n"
       "    (should= 42 (helper))\n"
       "    (helper)\n"
       "    (should= 42 (helper))))\n"))

(def report-fixture
  {:path "spec/foo_spec.clj"
   :structure-errors ["ERROR line 2: (it) inside (it) at line 1"]
   :parse-error nil
   :examples [{:describe-path ["math"]
               :name "adds"
               :scrap 9
               :raw-line-count 4
               :line-count 4
               :assertions 1
               :branches 0
               :setup-depth 0
               :with-redefs 0
               :helper-calls 0
               :helper-hidden-lines 0
               :smells []}]
   :blocks [{:path ["math"]
             :summary {:avg-scrap 9.0
                       :max-scrap 9
                       :example-count 1
                       :duplication-score 0}
             :worst-example {:name "adds"
                             :scrap 9}}]
   :summary {:avg-scrap 9.0
             :max-scrap 9
             :example-count 1
             :branching-examples 0
             :low-assertion-examples 1
             :zero-assertion-examples 0
             :with-redefs-examples 0
             :coverage-matrix-candidates 0
             :duplication-score 0
             :harmful-duplication-score 0
             :effective-duplication-score 0}})

(describe "scan-structure"
  (it "reports nested it forms"
    (let [errors (scrap/scan-structure
                   "(describe \"x\"\n  (it \"outer\"\n    (it \"inner\")))\n")]
      (should= 1 (count errors))
      (should-contain "(it) inside (it)" (first errors))))

  (it "reports unclosed forms"
    (let [errors (scrap/scan-structure
                   "(describe \"x\"\n  (it \"outer\"\n    (should= 1 1))")]
      (should (some #(clojure.string/includes? % "unclosed (describe)") errors)))))

(describe "analyze-source"
  (it "collects examples and summary data"
    (let [report (scrap/analyze-source
                   "(describe \"math\"\n  (it \"adds\"\n    (should= 3 (+ 1 2))))\n"
                   "spec/math_spec.clj")]
      (should= nil (:parse-error report))
      (should= [] (:structure-errors report))
      (should= 1 (count (:examples report)))
      (should= 1 (get-in report [:summary :example-count]))
      (should= 1 (get-in report [:summary :low-assertion-examples]))
      (should= ["math"] (-> report :blocks first :path))
      (should= 1 (-> report :blocks first :summary :example-count))))

  (it "scores complex workflow examples above the hotspot threshold"
    (let [report (scrap/analyze-source complex-workflow-source "spec/workflow_spec.clj")
          example (first (:examples report))]
      (should (< 20 (:scrap example)))))

  (it "reports multiple phases and mocking smells in complex workflow examples"
    (let [report (scrap/analyze-source complex-workflow-source "spec/workflow_spec.clj")
          example (first (:examples report))]
      (should-contain "multiple-phases" (:smells example))
      (should-contain "high-mocking" (:smells example))))

  (it "reports literal-heavy setup smells in complex workflow examples"
    (let [report (scrap/analyze-source complex-workflow-source "spec/workflow_spec.clj")
          example (first (:examples report))]
      (should-contain "literal-heavy-setup" (:smells example))))

  (it "records parse errors while preserving structure errors"
    (let [report (scrap/analyze-source
                   "(describe \"oops\"\n  (it \"bad\" [)\n"
                   "spec/bad_spec.clj")]
      (should (seq (:structure-errors report)))
      (should (string? (:parse-error report)))
      (should= [] (:examples report))))

  (it "derives fuzzy duplication metrics from repeated setup and arrange structure"
    (let [report (scrap/analyze-source
                   (str "(describe \"duplication\"\n"
                        "  (before (let [db {:port 5432 :host \"a\" :pool 4}] db))\n"
                        "  (it \"first\"\n"
                        "    (let [user {:id 1 :name \"alice\" :role :admin :team :blue :tier 1 :age 30 :city \"x\" :region \"y\" :active true :quota 9 :rank 2}]\n"
                        "      (service/run user))\n"
                        "    (should= :ok result))\n"
                        "  (it \"second\"\n"
                        "    (let [account {:id 2 :name \"bob\" :role :admin :team :blue :tier 2 :age 31 :city \"m\" :region \"n\" :active true :quota 8 :rank 3}]\n"
                        "      (service/run account))\n"
                        "    (should= :ok result)))\n")
                   "spec/duplication_spec.clj")
          summary (:summary report)]
      (should= 2 (:example-count summary))
      (should= 2 (:repeated-setup-examples summary))
      (should= 2 (:repeated-fixture-examples summary))
      (should= 2 (:repeated-literal-examples summary))
      (should= 2 (:repeated-arrange-examples summary))
      (should (< 0.0 (:avg-setup-similarity summary)))
      (should (< 0.0 (:avg-arrange-similarity summary)))
      (should (< 0.0 (:duplication-score summary))))))

  (it "recommends extractions only when repeated harmful structure has net benefit"
    (let [report (scrap/analyze-source
                   (str "(describe \"helpers\"\n"
                        "  (before (reset! state []))\n"
                        "  (it \"first\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"second\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"third\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"fourth\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"fifth\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result))))\n")
                   "spec/helpers_spec.clj")
          summary (:summary report)
          recommendation (first (:recommended-extractions summary))]
      (should= 1 (:recommended-extraction-count summary))
      (should (< 0.0 (:extraction-pressure-score summary)))
      (should= ["first" "second" "third" "fourth" "fifth"] (:it-names recommendation))
      (should= 3 (:line-start recommendation))
      (should (< (:line-start recommendation) (:line-end recommendation)))))

  (it "classifies repeated low-complexity examples as coverage-matrix candidates"
    (let [report (scrap/analyze-source
                   (str "(describe \"matrix\"\n"
                        "  (it \"first\"\n"
                        "    (let [result (core/validate-args [\"a\" \"--scan\"])]\n"
                        "      (should= true (:scan result))))\n"
                        "  (it \"second\"\n"
                        "    (let [result (core/validate-args [\"b\" \"--mutate-all\"])]\n"
                        "      (should= true (:mutate-all result)))))\n")
                   "spec/matrix_spec.clj")
          summary (:summary report)]
      (should= 2 (:coverage-matrix-candidates summary))
      (should (< (:effective-duplication-score summary) (:duplication-score summary)))))

  (it "classifies matrix-heavy files as safe to table-drive automatically"
    (let [report (scrap/analyze-source
                   (str "(describe \"matrix\"\n"
                        "  (it \"first\"\n"
                        "    (let [result (core/validate-args [\"a\" \"--scan\"])]\n"
                        "      (should= true (:scan result))))\n"
                        "  (it \"second\"\n"
                        "    (let [result (core/validate-args [\"b\" \"--mutate-all\"])]\n"
                        "      (should= true (:mutate-all result))))\n"
                        "  (it \"third\"\n"
                        "    (let [result (core/validate-args [\"c\" \"--reuse-lcov\"])]\n"
                        "      (should= true (:reuse-lcov result))))\n"
                        "  (it \"fourth\"\n"
                        "    (let [result (core/validate-args [\"d\" \"--update-manifest\"])]\n"
                        "      (should= true (:update-manifest result)))))\n")
                   "spec/matrix_spec.clj")
          output (scrap/render-report [report] false)]
      (should-contain "ai-actionability: AUTO_TABLE_DRIVE" output)
      (should-contain "Safe to table-drive automatically" output)))

  (it "renders recommended extractions with it names and line ranges"
    (let [report (scrap/analyze-source
                   (str "(describe \"helpers\"\n"
                        "  (before (reset! state []))\n"
                        "  (it \"first\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"second\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"third\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"fourth\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result)))\n"
                        "  (it \"fifth\"\n"
                        "    (let [result (service/run :ok)]\n"
                        "      (audit/log :ok result)\n"
                        "      (should= :ok result)\n"
                        "      (should-not= nil result))))\n")
                   "spec/helpers_spec.clj")
          output (scrap/render-report [report] false)]
      (should-contain "recommended-extractions:" output)
      (should-contain "first, second, third, fourth, fifth" output)
      (should-contain "line 3" output)))

  (it "charges helper-hidden complexity back to examples that call spec-local helpers"
    (let [report (scrap/analyze-source
                   (str "(describe \"helpers\"\n"
                        "  (defn temp-lcov []\n"
                        "    (let [tmp (java.io.File/createTempFile \"x\" \".info\")]\n"
                        "      (spit tmp \"alpha\")\n"
                        "      (slurp tmp)\n"
                        "      (spit tmp \"beta\")\n"
                        "      (slurp tmp)\n"
                        "      (.delete tmp)\n"
                        "      tmp))\n"
                        "  (it \"uses helper\"\n"
                        "    (temp-lcov)\n"
                        "    (should= 1 1)))\n")
                   "spec/helpers_spec.clj")
          example (first (:examples report))]
      (should (< 0 (:helper-hidden-lines example)))
      (should-contain "helper-hidden-complexity" (:smells example))))

  (it "treats merged table-driven api contract checks as low-pressure examples"
    (let [report (scrap/analyze-source
                   (str "(describe \"runner\"\n"
                        "  (it \"accepts explicit arities\"\n"
                        "    (let [dir-path \"target/x\"]\n"
                        "      (doseq [invoke! [(fn [] (runner/run-specs 100 dir-path))\n"
                        "                       (fn [] (runner/run-specs 100 dir-path \"clj -M:spec\"))]]\n"
                        "        (let [result (try\n"
                        "                       (invoke!)\n"
                        "                       :no-error\n"
                        "                       (catch Exception e\n"
                        "                         (if (instance? clojure.lang.ArityException e)\n"
                        "                           :arity-exception\n"
                        "                           :other-error)))]\n"
                        "          (should-not= :arity-exception result)))))\n")
                   "spec/runner_spec.clj")
          example (first (:examples report))]
      (should= true (:api-contract? example))
      (should-not-contain "low-assertion-density" (:smells example))
      (should-not-contain "large-example" (:smells example))
      (should (< (:scrap example) 35))))

  (it "can compare current reports to a saved baseline"
    (let [before (scrap/analyze-source
                   "(describe \"math\"\n  (it \"adds\"\n    (should= 3 (+ 1 2))))\n"
                   "spec/math_spec.clj")
          after (scrap/analyze-source
                  (str "(describe \"math\"\n"
                       "  (it \"adds\"\n"
                       "    (let [result (+ 1 2)]\n"
                       "      (should= 3 result)\n"
                       "      (should= true (number? result)))))\n")
                  "spec/math_spec.clj")
          baseline (scrap/baseline-document ["spec/math_spec.clj"] [before])
          compared (first (scrap/compare-reports baseline [after]))]
      (should= :mixed (get-in compared [:comparison :verdict]))
      (should (contains? (:comparison compared) :file-score-delta))))

  (it "classifies broad multi-hotspot files as split candidates"
    (let [source (str "(describe \"alpha\"\n"
                      "  (it \"first\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 1 (foo payload)))))\n"
                      "  (it \"second\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 2 (bar payload)))))\n"
                      "  (it \"third\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 3 (baz payload)))))\n"
                      "  (it \"fourth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 4 (qux payload)))))\n"
                      "  (it \"fifth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 5 (zap payload)))))\n"
                      "  (it \"sixth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 6 (zip payload))))))\n"
                      "(describe \"beta\"\n"
                      "  (it \"seventh\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 7 (zim payload)))))\n"
                      "  (it \"eighth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 8 (gir payload)))))\n"
                      "  (it \"ninth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 9 (lar payload)))))\n"
                      "  (it \"tenth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 10 (far payload)))))\n"
                      "  (it \"eleventh\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 11 (tar payload)))))\n"
                      "  (it \"twelfth\"\n"
                      "    (with-redefs [x inc]\n"
                      "      (let [payload {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11}]\n"
                      "        (should= 12 (jar payload))))))\n")
          report (scrap/analyze-source source "spec/broad_spec.clj")
          output (scrap/render-report [report] false)]
      (should-contain "remediation-mode: SPLIT" output)
      (should-contain "Split this spec file by responsibility before attempting local cleanup" output)))

(describe "collect-spec-files"
  (it "collects spec files from a directory tree"
    (let [root (java.io.File. "target/scrap/spec-tree")
          nested (java.io.File. root "nested")
          target (java.io.File. nested "example_spec.clj")
          ignored (java.io.File. nested "notes.txt")]
      (.mkdirs nested)
      (spit target "(describe \"x\")")
      (spit ignored "ignore")
      (let [files (scrap/collect-spec-files [(.getPath root)])]
        (should= [(.getPath target)] files))
      (.delete ignored)
      (.delete target)
      (.delete nested)
      (.delete root))))

(describe "render-report"
  (it "renders guidance by default"
    (let [output (scrap/render-report [report-fixture] false)]
      (should-contain "SCRAP Report" output)
      (should-contain "refactor-pressure:" output)
      (should-contain "ai-actionability:" output)
      (should-contain "why:" output)))

  (it "renders summary sections in default report mode"
    (let [output (scrap/render-report [report-fixture] false)]
      (should-contain "where:" output)
      (should-contain "how:" output)
      (should-contain "coverage-matrix-candidates:" output)
      (should-contain "HIGH:" output)
      (should-contain "Worst Examples" output)))

  (it "renders example names in default report mode"
    (let [output (scrap/render-report [report-fixture] false)]
      (should-contain "math / adds" output)))

  (it "includes full metrics in verbose mode"
    (let [output (scrap/render-report [report-fixture] true)]
      (should-contain "structure-errors" output)
      (should-contain "blocks:" output)
      (should-contain "avg-scrap:" output)
      (should-contain "duplication-score:" output)
      (should-contain "coverage-matrix-candidates:" output))))

  (it "renders a comparison section when a baseline is attached"
    (let [output (scrap/render-report
                   [(assoc report-fixture
                           :structure-errors []
                           :comparison {:verdict :worse
                                        :file-score-delta 5.0
                                        :avg-scrap-delta 1.0
                                        :max-scrap-delta 2
                                        :harmful-duplication-delta 1
                                        :case-matrix-delta 0
                                        :helper-hidden-delta 1})]
                   false)]
      (should-contain "comparison:" output)
      (should-contain "verdict: worse" output)
      (should-contain "Refactor appears negative" output)))

(describe "cli"
  (it "prints usage for --help on stdout"
    (let [{:keys [exit-code stdout]} (scrap/run-cli ["--help"])]
      (should= 0 exit-code)
      (should-contain "Usage: clj -M:scrap" stdout)
      (should-contain "--help" stdout)
      (should-contain "--compare PATH" stdout)))

  (it "exposes the same usage text from the usage helper"
    (let [stdout (scrap/usage)]
      (should-contain "If no path is provided, SCRAP defaults to spec." stdout)
      (should-contain "--write-baseline" stdout)
      (should-contain "--json" stdout))))
