(ns scrap.scan-nesting)

(defn- nesting-error
  [line form parent]
  (str "ERROR line " line ": (" form ") inside (" (:form parent) ") at line " (:line parent)))

(defn- invalid-parent-child?
  [parent-form form]
  (or (= parent-form "it")
      (and (= parent-form "describe") (= form "describe"))
      (and (= parent-form "context") (= form "describe"))
      (and (= parent-form "it") (#{"before" "with-stubs" "around" "with" "context"} form))))

(defn validate-nesting
  [form line form-stack]
  (when-let [parent (peek form-stack)]
    (when (invalid-parent-child? (:form parent) form)
      (nesting-error line form parent))))

(defn pop-form
  [form-stack]
  (let [completed (peek form-stack)
        stack (pop form-stack)]
    (assoc completed :stack stack)))

(defn unclosed-errors
  [result]
  (let [eof-line (:line result)]
    (mapv (fn [entry]
            (str "ERROR line " eof-line ": unclosed (" (:form entry)
                 ") from line " (:line entry)))
          (:form-stack result))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:27:09.354175-05:00", :module-hash "965317993", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-926980280"} {:id "defn-/nesting-error", :kind "defn-", :line 3, :end-line 5, :hash "-1659649396"} {:id "defn-/invalid-parent-child?", :kind "defn-", :line 7, :end-line 12, :hash "979130391"} {:id "defn/validate-nesting", :kind "defn", :line 14, :end-line 18, :hash "-344232348"} {:id "defn/pop-form", :kind "defn", :line 20, :end-line 24, :hash "1140559298"} {:id "defn/unclosed-errors", :kind "defn", :line 26, :end-line 32, :hash "-1639534963"}]}
;; clj-mutate-manifest-end
