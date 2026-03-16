(ns scrap.guidance
  (:require [scrap.actionability :as actionability]
            [scrap.comparison :as comparison]
            [scrap.pressure :as pressure]))

(def ratio pressure/ratio)
(def stable-summary? pressure/stable-summary?)
(def refactor-pressure-score pressure/refactor-pressure-score)
(def pressure-level pressure/pressure-level)
(def remediation-mode pressure/remediation-mode)
(def guidance actionability/guidance)
(def compare-reports comparison/compare-reports)
