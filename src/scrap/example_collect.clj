(ns scrap.example-collect
  (:require [scrap.example-helpers :as helpers]
            [scrap.example-score :as score]
            [scrap.expr :as expr]))

(defn collect-examples
  [forms]
  (let [helper-context (helpers/helper-context forms)]
    (letfn [(walk [forms describe-path inherited-setup-forms]
              (let [local-setup (expr/setup-forms forms)
                    effective-setup (vec (concat inherited-setup-forms local-setup))]
                (mapcat
                  (fn [form]
                    (let [head (expr/seq-head form)]
                      (cond
                        (#{'describe 'context} head)
                        (let [[_ name & body] form]
                          (walk body (conj describe-path name) effective-setup))

                        (= 'it head)
                        [(score/score-example form helper-context effective-setup describe-path)]

                        :else [])))
                  forms)))]
      (walk forms [] []))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:44:32.213927-05:00", :module-hash "-2041924587", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "-444734460"} {:id "defn/collect-examples", :kind "defn", :line 6, :end-line 25, :hash "555908238"}]}
;; clj-mutate-manifest-end
