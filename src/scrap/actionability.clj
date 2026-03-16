(ns scrap.actionability
  (:require [scrap.actionability-modes :as modes]
            [scrap.actionability-rules :as rules]
            [scrap.judgment :as judgment]
            [scrap.policy :as policy]
            [scrap.pressure :as pressure]
            [scrap.report-model :as report-model]))

(defn guidance
  [{:keys [summary blocks examples]}]
  (let [{:keys [file-score file-level remediation-mode ratios harmful-duplication]}
        (judgment/summary-context summary blocks)
        sorted-blocks (sort-by #(pressure/refactor-pressure-score (:summary %)) > blocks)
        sorted-examples (sort-by :scrap > examples)
        actionability (modes/ai-actionability summary blocks)
        split-rule (rules/split-rule remediation-mode)
        actions (if (= remediation-mode "STABLE")
                  [{:confidence 3
                    :label "HIGH"
                    :text "No refactor recommended; the file is structurally stable enough to leave alone."}]
                  (->> (cons split-rule
                             (rules/local-action-rules summary
                                                       remediation-mode
                                                       (:zero-assertion-ratio ratios)
                                                       (:low-assertion-ratio ratios)
                                                       (:branching-ratio ratios)
                                                       (:mocking-ratio ratios)
                                                       harmful-duplication))
                       (remove nil?)
                       (sort-by (juxt (comp - :confidence) :text))
                       vec))]
    (report-model/guidance-report
      {:file-score file-score
       :file-level file-level
       :remediation-mode remediation-mode
       :ai-actionability (:mode actionability)
       :ai-actionability-message (:message actionability)
       :actions (vec (take (:max-actions policy/actionability) actions))
       :top-blocks (vec (take (:top-block-count policy/report) sorted-blocks))
       :top-examples (vec (take (:top-example-count policy/report) sorted-examples))})))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:38:15.626955-05:00", :module-hash "1827921105", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "1342746636"} {:id "defn/guidance", :kind "defn", :line 7, :end-line 37, :hash "-1971073862"}]}
;; clj-mutate-manifest-end
