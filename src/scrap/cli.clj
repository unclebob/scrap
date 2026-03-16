(ns scrap.cli
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [scrap.analyze :as analyze]
            [scrap.guidance :as guidance]
            [scrap.report :as report]))

(defn- baseline-output-path
  [paths]
  (let [roots (if (seq paths) paths ["spec"])
        name-part (-> (clojure.string/join "_" roots)
                      (clojure.string/replace #"[^A-Za-z0-9._-]+" "_")
                      (clojure.string/replace #"^_+" "")
                      (clojure.string/replace #"_+$" ""))]
    (str "target/scrap/" (if (seq name-part) name-part "spec") ".json")))

(defn- write-baseline!
  [path doc]
  (.mkdirs (io/file "target/scrap"))
  (spit path (json/write-str doc))
  path)

(defn- read-json-file
  [path]
  (json/read-str (slurp path) :key-fn keyword))

(defn- apply-arg
  [opts remaining]
  (let [arg (first remaining)]
    (case arg
      "--help" [(assoc opts :help true) (rest remaining)]
      "--verbose" [(assoc opts :verbose true) (rest remaining)]
      "--json" [(assoc opts :json true) (rest remaining)]
      "--write-baseline" [(assoc opts :write-baseline true) (rest remaining)]
      "--compare" [(assoc opts :compare (second remaining)) (nnext remaining)]
      [(update opts :paths conj arg) (rest remaining)])))

(defn- parse-args
  [args]
  (loop [opts {:help false
               :verbose false
               :json false
               :write-baseline false
               :compare nil
               :paths []}
         remaining args]
    (if (seq remaining)
      (let [[next-opts next-remaining] (apply-arg opts remaining)]
        (recur next-opts next-remaining))
      opts)))

(defn usage
  []
  (str
    "Usage: clj -M:scrap [path ...] [options]\n\n"
    "If no path is provided, SCRAP defaults to spec.\n\n"
    "Options:\n"
    "  --help            Print this usage text.\n"
    "  --verbose         Show full per-file, block, and example metrics.\n"
    "  --json            Emit the report as JSON.\n"
    "  --write-baseline  Write a baseline report under target/scrap/.\n"
    "  --compare PATH    Compare the current report to a saved baseline JSON file.\n"))

(defn run-cli
  [args]
  (let [{:keys [help paths verbose json write-baseline compare]} (parse-args args)]
    (if help
      {:exit-code 0
       :stdout (usage)}
      (let [files (analyze/collect-spec-files paths)
            reports (mapv analyze/analyze-file files)
            reports (if compare
                      (guidance/compare-reports (read-json-file compare) reports)
                      reports)
            baseline-doc (analyze/baseline-document paths reports)
            baseline-path (baseline-output-path paths)
            has-errors? (some #(or (seq (:structure-errors %)) (:parse-error %)) reports)
            stdout (str
                     (if json
                       (report/render-json paths reports)
                       (report/render-report reports verbose))
                     (when write-baseline
                       (str "\nBaseline written: " baseline-path)))]
        (when write-baseline
          (write-baseline! baseline-path baseline-doc))
        {:exit-code (if has-errors? 1 0)
         :stdout stdout}))))
