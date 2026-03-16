(ns scrap.core
  (:require [scrap.analyze :as analyze]
            [scrap.cli :as cli]
            [scrap.guidance :as guidance]
            [scrap.report :as report]
            [scrap.scan :as scan]))

(def scan-structure scan/scan-structure)
(def analyze-source analyze/analyze-source)
(def collect-spec-files analyze/collect-spec-files)
(def baseline-document analyze/baseline-document)
(def compare-reports guidance/compare-reports)
(def render-report report/render-report)
(def usage cli/usage)
(def run-cli cli/run-cli)

(defn -main
  [& args]
  (let [{:keys [exit-code stdout]} (cli/run-cli args)]
    (println stdout)
    (shutdown-agents)
    (System/exit exit-code)))
