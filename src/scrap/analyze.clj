(ns scrap.analyze
  (:require [scrap.source :as source]))

(def analyze-source source/analyze-source)
(def analyze-file source/analyze-file)
(def collect-spec-files source/collect-spec-files)
(def baseline-document source/baseline-document)

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:41:14.540503-05:00", :module-hash "-930506754", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-1535039321"} {:id "def/analyze-source", :kind "def", :line 4, :end-line 4, :hash "1662465864"} {:id "def/analyze-file", :kind "def", :line 5, :end-line 5, :hash "533296123"} {:id "def/collect-spec-files", :kind "def", :line 6, :end-line 6, :hash "-547052805"} {:id "def/baseline-document", :kind "def", :line 7, :end-line 7, :hash "-240175552"}]}
;; clj-mutate-manifest-end
