(ns scrap.report-model
  (:require [scrap.policy :as policy]))

(defn block-report
  [{:keys [path summary worst-example]}]
  {:path (vec path)
   :summary (or summary {})
   :worst-example worst-example})

(defn file-report
  [{:keys [path content-hash structure-errors parse-error examples summary blocks comparison]}]
  {:path path
   :content-hash content-hash
   :structure-errors (vec (or structure-errors []))
   :parse-error parse-error
   :examples (vec (or examples []))
   :summary summary
   :blocks (vec (or blocks []))
   :comparison comparison})

(defn guidance-report
  [{:keys [file-score file-level remediation-mode ai-actionability ai-actionability-message actions top-blocks top-examples]}]
  {:file-score file-score
   :file-level file-level
   :remediation-mode remediation-mode
   :ai-actionability ai-actionability
   :ai-actionability-message ai-actionability-message
   :actions (vec (or actions []))
   :top-blocks (vec (or top-blocks []))
   :top-examples (vec (or top-examples []))})

(defn baseline-document
  [paths reports]
  {:baseline-version (:baseline-version policy/report)
   :paths (vec paths)
   :reports (mapv #(select-keys % [:path :content-hash :summary :structure-errors :parse-error]) reports)})
