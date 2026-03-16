(ns scrap.scan
  (:require [scrap.scan-nesting :as nesting]
            [scrap.scan-tokenize :as tokenize]
            [scrap.shared :as shared]))

(defn- open-form-state
  [state token old-depth]
  (if (and token (shared/speclj-forms token))
    (let [error (nesting/validate-nesting token (:line state) (:form-stack state))]
      (cond-> state
        error (update :errors conj error)
        true (update :form-stack conj {:form token :line (:line state) :depth old-depth})))
    state))

(defn- closing-form?
  [state old-mode c old-depth new-depth]
  (and (= old-mode :normal)
       (= c \))
       (< new-depth old-depth)
       (seq (:form-stack state))
       (= new-depth (:depth (peek (:form-stack state))))))

(defn- scan-char-step
  [chars n]
  (fn [state i]
    (let [c (nth chars i)
          next-c (when (< (inc i) n) (nth chars (inc i)))
          old-depth (:depth state)
          old-mode (:mode state)
          state (tokenize/process-char state c next-c)
          new-depth (:depth state)]
      (cond
        (and (= old-mode :normal) (= c \() (> new-depth old-depth))
        (open-form-state state (tokenize/extract-token chars i) old-depth)

        (closing-form? state old-mode c old-depth new-depth)
        (assoc state :form-stack (:stack (nesting/pop-form (:form-stack state))))

        :else state))))

(defn scan-structure
  [text]
  (let [chars (vec text)
        n (count chars)
        init {:mode :normal :depth 0 :line 1 :escape false :skip false :errors [] :form-stack []}
        result (reduce (scan-char-step chars n) init (range n))]
    (into (:errors result) (nesting/unclosed-errors result))))
