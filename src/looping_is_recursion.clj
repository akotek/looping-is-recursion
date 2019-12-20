(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= (count a-seq) 1) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) acc
      :else (recur (inc acc) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         seq1 a-seq]
    (cond
      (and (empty? seq1) (zero? n)) 0
      (empty? seq1) (/ sum n)
      :else (recur (+ sum (first seq1)) (inc n) (rest seq1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq1 a-seq
         set1 #{}]
    (if (empty? seq1)
      set1
      (recur (rest seq1)
             (toggle set1 (first seq1))))))

(defn fast-fibo [n]
  (loop [f-n1 0
         f-n 1
         k 1]
    (cond
      (= k n) f-n
      (= n 0) 0
      :else (recur f-n (+ f-n1 f-n) (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq []
         go-seq a-seq]
    (let [f (first go-seq)]
      (if (or (some (fn [x] (= x f)) new-seq) (empty? go-seq))
        new-seq
        (recur (conj new-seq f) (rest go-seq))))))
