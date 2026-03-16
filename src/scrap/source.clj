(ns scrap.source
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [scrap.examples :as examples]
            [scrap.scan :as scan]
            [scrap.shared :as shared]
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
      {:path path
       :content-hash (content-hash source-text)
       :structure-errors structure-errors
       :parse-error (:parse-error forms)
       :examples []}
      (let [examples (vec (examples/collect-examples forms))
            summary (summary/summarize-examples examples)]
        {:path path
         :content-hash (content-hash source-text)
         :structure-errors structure-errors
         :parse-error nil
         :examples examples
         :summary summary
         :blocks (summary/summarize-blocks examples)}))))

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
  {:baseline-version shared/baseline-version
   :paths (vec paths)
   :reports (mapv #(select-keys % [:path :content-hash :summary :structure-errors :parse-error]) reports)})
