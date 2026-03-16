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
