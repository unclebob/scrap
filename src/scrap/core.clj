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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:44:09.087423-05:00", :module-hash "913275543", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "2093465913"} {:id "def/scan-structure", :kind "def", :line 8, :end-line 8, :hash "2138974453"} {:id "def/analyze-source", :kind "def", :line 9, :end-line 9, :hash "630348821"} {:id "def/collect-spec-files", :kind "def", :line 10, :end-line 10, :hash "-1763563063"} {:id "def/baseline-document", :kind "def", :line 11, :end-line 11, :hash "97411124"} {:id "def/compare-reports", :kind "def", :line 12, :end-line 12, :hash "475084449"} {:id "def/render-report", :kind "def", :line 13, :end-line 13, :hash "937959618"} {:id "def/usage", :kind "def", :line 14, :end-line 14, :hash "-1114522022"} {:id "def/run-cli", :kind "def", :line 15, :end-line 15, :hash "-2114273884"} {:id "defn/-main", :kind "defn", :line 17, :end-line 22, :hash "-530404932"}]}
;; clj-mutate-manifest-end
