(ns scrap.shared)

(def assertion-heads
  '#{should should= should-not should-not= should-contain should-not-contain
     should-be-nil should-not-be-nil should-throw})

(def branch-heads
  '#{if if-not when when-not cond case and or try loop while})

(def table-branch-heads
  '#{doseq for every? map mapv run!})

(def setup-heads
  '#{let binding with-redefs before before-all around with with-stubs})

(def helper-def-heads
  '#{defn defn- defmacro})

(def speclj-form-symbols
  '#{describe context it before before-all after with-stubs with around run-specs})

(def speclj-forms
  (set (map name speclj-form-symbols)))

(def default-top-example-count 5)
(def default-top-block-count 3)
(def duplication-threshold 0.5)
(def baseline-version 1)
(def complexity-cap 25.0)
(def complexity-rise-rate 0.18)
(def trivial-complexity-floor 1.0)

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:29:23.191723-05:00", :module-hash "-1988699613", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "820902174"} {:id "def/assertion-heads", :kind "def", :line 3, :end-line 5, :hash "-1065444453"} {:id "def/branch-heads", :kind "def", :line 7, :end-line 8, :hash "939441433"} {:id "def/table-branch-heads", :kind "def", :line 10, :end-line 11, :hash "66148137"} {:id "def/setup-heads", :kind "def", :line 13, :end-line 14, :hash "1350497902"} {:id "def/helper-def-heads", :kind "def", :line 16, :end-line 17, :hash "-1922067655"} {:id "def/speclj-form-symbols", :kind "def", :line 19, :end-line 20, :hash "-1377081086"} {:id "def/speclj-forms", :kind "def", :line 22, :end-line 23, :hash "-556794520"} {:id "def/default-top-example-count", :kind "def", :line 25, :end-line 25, :hash "-1051415547"} {:id "def/default-top-block-count", :kind "def", :line 26, :end-line 26, :hash "1021191695"} {:id "def/duplication-threshold", :kind "def", :line 27, :end-line 27, :hash "-1662471093"} {:id "def/baseline-version", :kind "def", :line 28, :end-line 28, :hash "2045562062"} {:id "def/complexity-cap", :kind "def", :line 29, :end-line 29, :hash "558391522"} {:id "def/complexity-rise-rate", :kind "def", :line 30, :end-line 30, :hash "1125097759"} {:id "def/trivial-complexity-floor", :kind "def", :line 31, :end-line 31, :hash "1589176131"}]}
;; clj-mutate-manifest-end
