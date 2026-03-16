(ns scrap.actionability
  (:require [scrap.actionability-modes :as modes]
            [scrap.actionability-rules :as rules]
            [scrap.pressure :as pressure]
            [scrap.shared :as shared]))

(defn guidance
  [{:keys [summary blocks examples]}]
  (let [file-score (pressure/refactor-pressure-score summary)
        sorted-blocks (sort-by #(pressure/refactor-pressure-score (:summary %)) > blocks)
        sorted-examples (sort-by :scrap > examples)
        mode (pressure/remediation-mode summary blocks)
        actionability (modes/ai-actionability summary blocks)
        example-count (or (:example-count summary) 0)
        low-assertion-ratio (pressure/ratio (or (:low-assertion-examples summary) 0) example-count)
        zero-assertion-ratio (pressure/ratio (or (:zero-assertion-examples summary) 0) example-count)
        branching-ratio (pressure/ratio (or (:branching-examples summary) 0) example-count)
        mocking-ratio (pressure/ratio (or (:with-redefs-examples summary) 0) example-count)
        harmful-duplication (or (:effective-duplication-score summary) 0)
        split-rule (rules/split-rule mode)
        actions (if (= mode "STABLE")
                  [{:confidence 3
                    :label "HIGH"
                    :text "No refactor recommended; the file is structurally stable enough to leave alone."}]
                  (->> (cons split-rule
                             (rules/local-action-rules summary mode zero-assertion-ratio low-assertion-ratio branching-ratio mocking-ratio harmful-duplication))
                       (remove nil?)
                       (sort-by (juxt (comp - :confidence) :text))
                       vec))]
    {:file-score file-score
     :file-level (pressure/pressure-level summary)
     :remediation-mode mode
     :ai-actionability (:mode actionability)
     :ai-actionability-message (:message actionability)
     :actions (vec (take 4 actions))
     :top-blocks (vec (take shared/default-top-block-count sorted-blocks))
     :top-examples (vec (take shared/default-top-example-count sorted-examples))}))
