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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:00:58.166992-05:00", :module-hash "747072774", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "-347896172"} {:id "def/ratio", :kind "def", :line 6, :end-line 6, :hash "1983814379"} {:id "def/stable-summary?", :kind "def", :line 7, :end-line 7, :hash "142717210"} {:id "def/refactor-pressure-score", :kind "def", :line 8, :end-line 8, :hash "888127851"} {:id "def/pressure-level", :kind "def", :line 9, :end-line 9, :hash "-757474751"} {:id "def/remediation-mode", :kind "def", :line 10, :end-line 10, :hash "-1494428904"} {:id "def/guidance", :kind "def", :line 11, :end-line 11, :hash "-161473645"} {:id "def/compare-reports", :kind "def", :line 12, :end-line 12, :hash "80470690"}]}
;; clj-mutate-manifest-end
