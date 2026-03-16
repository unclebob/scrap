(ns scrap.pressure
  (:require [scrap.pressure-mode :as mode]
            [scrap.pressure-score :as score]
            [scrap.pressure-stability :as stability]))

(def ratio stability/ratio)
(def stable-summary? stability/stable-summary?)
(def refactor-pressure-score score/refactor-pressure-score)
(def pressure-level score/pressure-level)
(def remediation-mode mode/remediation-mode)

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:02:29.741151-05:00", :module-hash "-1535147360", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "1870058219"} {:id "def/ratio", :kind "def", :line 6, :end-line 6, :hash "1537289170"} {:id "def/stable-summary?", :kind "def", :line 7, :end-line 7, :hash "1306732389"} {:id "def/refactor-pressure-score", :kind "def", :line 8, :end-line 8, :hash "783520897"} {:id "def/pressure-level", :kind "def", :line 9, :end-line 9, :hash "1536263347"} {:id "def/remediation-mode", :kind "def", :line 10, :end-line 10, :hash "-1785387292"}]}
;; clj-mutate-manifest-end
