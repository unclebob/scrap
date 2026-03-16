(ns scrap.pressure
  (:require [scrap.pressure-mode :as mode]
            [scrap.pressure-score :as score]
            [scrap.pressure-stability :as stability]))

(def ratio stability/ratio)
(def stable-summary? stability/stable-summary?)
(def refactor-pressure-score score/refactor-pressure-score)
(def pressure-level score/pressure-level)
(def remediation-mode mode/remediation-mode)
