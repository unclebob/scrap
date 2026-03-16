(ns scrap.report-model
  (:require [clojure.spec.alpha :as s]
            [scrap.policy :as policy]))

(s/def ::path string?)
(s/def ::block-path (s/coll-of string? :kind vector?))
(s/def ::content-hash string?)
(s/def ::structure-errors (s/coll-of string? :kind vector?))
(s/def ::parse-error (s/nilable string?))
(s/def ::examples vector?)
(s/def ::summary (s/nilable map?))
(s/def ::blocks vector?)
(s/def ::comparison (s/nilable map?))
(s/def ::analysis-report
  (s/keys :req-un [::path ::content-hash ::structure-errors ::parse-error ::examples ::blocks]
          :opt-un [::summary ::comparison]))

(s/def ::worst-example map?)
(s/def ::block-report* (s/keys :req-un [::block-path ::summary ::worst-example]))

(s/def ::file-score number?)
(s/def ::file-level #{"STABLE" "LOW" "MEDIUM" "HIGH" "CRITICAL"})
(s/def ::remediation-mode #{"STABLE" "LOCAL" "SPLIT"})
(s/def ::ai-actionability string?)
(s/def ::ai-actionability-message string?)
(s/def ::actions vector?)
(s/def ::top-blocks vector?)
(s/def ::top-examples vector?)
(s/def ::guidance-result
  (s/keys :req-un [::file-score
                   ::file-level
                   ::remediation-mode
                   ::ai-actionability
                   ::ai-actionability-message
                   ::actions
                   ::top-blocks
                   ::top-examples]))

(s/def ::baseline-version int?)
(s/def ::paths (s/coll-of string? :kind vector?))
(s/def ::reports vector?)
(s/def ::baseline-report
  (s/keys :req-un [::baseline-version ::paths ::reports]))

(defn- validate!
  [spec value label]
  (when-not (s/valid? spec value)
    (throw (ex-info (str "Invalid report model: " label)
                    {:label label
                     :explain (s/explain-data spec value)})))
  value)

(defn block-report
  [{:keys [path summary worst-example]}]
  (let [block {:path (vec path)
               :summary (or summary {})
               :worst-example worst-example}]
    (validate! ::block-report*
               {:block-path (:path block)
                :summary (:summary block)
                :worst-example (:worst-example block)}
               "block-report")
    block))

(defn analysis-report
  [{:keys [path content-hash structure-errors parse-error examples summary blocks comparison]}]
  (validate! ::analysis-report
             {:path path
              :content-hash content-hash
              :structure-errors (vec (or structure-errors []))
              :parse-error parse-error
              :examples (vec (or examples []))
              :summary summary
              :blocks (vec (or blocks []))
              :comparison comparison}
             "analysis-report"))

(defn file-report
  [report]
  (analysis-report report))

(defn guidance-report
  [{:keys [file-score file-level remediation-mode ai-actionability ai-actionability-message actions top-blocks top-examples]}]
  (validate! ::guidance-result
             {:file-score file-score
              :file-level file-level
              :remediation-mode remediation-mode
              :ai-actionability ai-actionability
              :ai-actionability-message ai-actionability-message
              :actions (vec (or actions []))
              :top-blocks (vec (or top-blocks []))
              :top-examples (vec (or top-examples []))}
             "guidance-report"))

(defn baseline-document
  [paths reports]
  (validate! ::baseline-report
             {:baseline-version (:baseline-version policy/report-policy)
              :paths (vec paths)
              :reports (mapv #(select-keys % [:path :content-hash :summary :structure-errors :parse-error]) reports)}
             "baseline-document"))
