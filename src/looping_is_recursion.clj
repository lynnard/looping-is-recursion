(ns looping-is-recursion)

(defn power [base exp]
  ((fn [acc exp]
    (if (== exp 0)
      acc
      (recur (* acc base) (dec exp))))
    (if (== base 0) 0 1) exp))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [ind 0
         sq a-seq]
    (cond
      (empty? sq) nil
      (pred (first sq)) ind
      :else (recur (inc ind) (rest sq)))))

(defn avg [a-seq]
  (loop [ct 0
         sum 0
         sq a-seq]
    (if (empty? sq)
      (/ sum ct)
      (recur (inc ct) (+ sum (first sq)) (rest sq)))))

(defn parity [a-seq]
  (loop [acc #{}
         sq a-seq]
    (cond
      (empty? sq) acc
      (contains? acc (first sq)) (recur (disj acc (first sq)) (rest sq))
      :else (recur (conj acc (first sq)) (rest sq)))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         curr n]
    (if (zero? curr)
      a
      (recur b (+ a b) (dec curr)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         acc '()
         sq a-seq]
    (let [h (first sq)]
      (if (or (empty? sq)
              (contains? seen h))
        (reverse acc)
        (recur (conj seen h) (cons h acc) (rest sq))))))
