(ns scrap.source
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [scrap.examples :as examples]
            [scrap.report-model :as report-model]
            [scrap.scan :as scan]
            [scrap.summary :as summary]))

(defn- content-hash
  [s]
  (format "%08x" (bit-and 0xffffffff (hash s))))

(defn- read-source-forms
  [source-str]
  (let [rdr (reader-types/source-logging-push-back-reader source-str)
        opts {:read-cond :allow :features #{:clj} :eof ::eof}]
    (loop [forms []]
      (let [form (reader/read opts rdr)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn analyze-source
  [source-text path]
  (let [structure-errors (scan/scan-structure source-text)
        forms (try
                (read-source-forms source-text)
                (catch Exception ex
                  {:parse-error (.getMessage ex)}))]
    (if (map? forms)
      (report-model/file-report
        {:path path
         :content-hash (content-hash source-text)
         :structure-errors structure-errors
         :parse-error (:parse-error forms)
         :examples []})
      (let [examples (vec (examples/collect-examples forms))
            summary (summary/summarize-examples examples)]
        (report-model/file-report
          {:path path
           :content-hash (content-hash source-text)
           :structure-errors structure-errors
           :parse-error nil
           :examples examples
           :summary summary
           :blocks (summary/summarize-blocks examples)})))))

(defn analyze-file
  [path]
  (analyze-source (slurp path) path))

(defn- spec-file?
  [^java.io.File f]
  (and (.isFile f)
       (or (str/ends-with? (.getName f) "_spec.clj")
           (str/ends-with? (.getName f) "_spec.cljc"))))

(defn collect-spec-files
  [paths]
  (let [roots (if (seq paths) paths ["spec"])]
    (->> roots
         (map io/file)
         (mapcat (fn [f]
                   (cond
                     (.isFile f) [f]
                     (.isDirectory f) (filter spec-file? (file-seq f))
                     :else [])))
         (filter spec-file?)
         (sort-by #(.getPath ^java.io.File %))
         (mapv #(.getPath ^java.io.File %)))))

(defn baseline-document
  [paths reports]
  (report-model/baseline-document paths reports))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T14:34:54.012797-05:00", :module-hash "611566442", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 9, :hash "-1997847383"} {:id "defn-/content-hash", :kind "defn-", :line 11, :end-line 13, :hash "-1840176928"} {:id "defn-/read-source-forms", :kind "defn-", :line 15, :end-line 23, :hash "-202061258"} {:id "defn/analyze-source", :kind "defn", :line 25, :end-line 48, :hash "255674488"} {:id "defn/analyze-file", :kind "defn", :line 50, :end-line 52, :hash "-322304566"} {:id "defn-/spec-file?", :kind "defn-", :line 54, :end-line 58, :hash "1159954305"} {:id "defn/collect-spec-files", :kind "defn", :line 60, :end-line 72, :hash "-798726208"} {:id "defn/baseline-document", :kind "defn", :line 74, :end-line 76, :hash "617379355"}]}
;; clj-mutate-manifest-end
