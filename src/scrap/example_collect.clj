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
