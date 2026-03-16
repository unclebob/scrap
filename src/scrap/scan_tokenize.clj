(ns scrap.scan-tokenize)

(defn- process-quoted-char
  [state c]
  (cond
    (= c \\) (assoc state :escape true)
    (= c \") (assoc state :mode :normal)
    (= c \newline) (update state :line inc)
    :else state))

(defn- process-normal-char
  [state c next-c]
  (cond
    (= c \;) (assoc state :mode :comment)
    (= c \\) (assoc state :escape true)
    (= c \") (assoc state :mode :string)
    (and (= c \#) (= next-c \")) (assoc state :mode :regex :skip true)
    (= c \newline) (update state :line inc)
    (= c \() (update state :depth inc)
    (= c \)) (update state :depth dec)
    :else state))

(defn process-char
  [state c next-c]
  (let [{:keys [mode line escape skip]} state]
    (cond
      skip (assoc state :skip false)
      escape (assoc state :escape false)
      (= mode :comment) (if (= c \newline)
                          (assoc state :mode :normal :line (inc line))
                          state)
      (= mode :string) (process-quoted-char state c)
      (= mode :regex) (process-quoted-char state c)
      :else (process-normal-char state c next-c))))

(def token-delimiters #{\space \newline \tab \( \) \"})

(defn extract-token
  [chars i]
  (let [n (count chars)
        start (inc i)]
    (when (< start n)
      (let [end (reduce (fn [_ j]
                          (if (token-delimiters (nth chars j))
                            (reduced j)
                            (inc j)))
                        start
                        (range start n))]
        (when (> end start)
          (apply str (subvec chars start end)))))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T09:28:09.525033-05:00", :module-hash "1349267512", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "1158996254"} {:id "defn-/process-quoted-char", :kind "defn-", :line 3, :end-line 9, :hash "261889984"} {:id "defn-/process-normal-char", :kind "defn-", :line 11, :end-line 21, :hash "1652627660"} {:id "defn/process-char", :kind "defn", :line 23, :end-line 34, :hash "685885996"} {:id "def/token-delimiters", :kind "def", :line 36, :end-line 36, :hash "1843939576"} {:id "defn/extract-token", :kind "defn", :line 38, :end-line 50, :hash "1230788345"}]}
;; clj-mutate-manifest-end
