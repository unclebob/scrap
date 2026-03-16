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
