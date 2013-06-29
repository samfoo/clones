(ns clones.cpu)

(defn bit-set? [x mask] (= (bit-and x mask) mask))

(defn make-cpu [] {:a 0, :x 0 :y 0, :sp 0 :p 0 :pc 0})

(defn update-flags [cpu new-flags] (assoc cpu :p new-flags))

(defn set-flag [flags mask] (bit-or flags mask))
(defn unset-flag [flags mask] (bit-and flags (bit-not mask)))
(defn flag? [flags mask] (bit-set? flags mask))

(defn unsigned-byte [b] (bit-and 0xff b))

(defn set-flags
  [flags items]
  (reduce (fn [memo, pair]
    (let [matches? (second pair)
          flag (first pair)]
      (if matches?
        (set-flag memo flag)
        (unset-flag memo flag))))
    flags
    items))

(defn negative? [b] (== 0x80 (bit-and b 0x80)))

(def carry-flag 0x01)
(def zero-flag 0x02)
(def overflow-flag 0x40)
(def negative-flag 0x80)

(defn carry-flag? [cpu] (flag? (:p cpu) carry-flag))
(defn zero-flag? [cpu] (flag? (:p cpu) zero-flag))
(defn overflow-flag? [cpu] (flag? (:p cpu) overflow-flag))
(defn negative-flag? [cpu] (flag? (:p cpu) negative-flag))

(defn subtract-overflowed?
  [orig arg result]
  (let [orig-neg? (bit-set? orig 0x80)
        arg-neg? (bit-set? arg 0x80)
        result-neg? (bit-set? result 0x80)]
    (if (and (not orig-neg?) arg-neg? result-neg?)
      ;; Subtracting a negative from a positive shouldn't result in a negative
      true
      (if (and orig-neg? (not arg-neg?) (not result-neg?))
        ;; Subtracking a positive from a negative shouldn't result in a
        ;; positive
        true
        false))))

(defn add-overflowed?
  [orig arg result]
  (let [orig-neg? (bit-set? orig 0x80)
        arg-neg? (bit-set? arg 0x80)
        result-neg? (bit-set? result 0x80)]
    (if (and (not orig-neg?) (not arg-neg?) result-neg?)
      ;; Adding two positives should not result in a negative
      true
      (if (and orig-neg? arg-neg? (not result-neg?))
        ;; Adding two negatives should not result in a positive
        true
        false))))

(defn adc
  [cpu arg]
  (let [result (unsigned-byte (if (carry-flag? cpu)
                 (+ (:a cpu) arg 1)
                 (+ (:a cpu) arg)))
        flags (:p cpu)
        carried? (< result (:a cpu))
        overflowed? (add-overflowed? (:a cpu) arg result)
        updates {carry-flag carried?
                 overflow-flag overflowed?
                 negative-flag (negative? result)
                 zero-flag (zero? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:a result :p new-flags})))

(defn sbc
  [cpu arg]
  (let [result (unsigned-byte (if (carry-flag? cpu)
                 (- (:a cpu) arg)
                 (- (:a cpu) arg 1)))
        flags (:p cpu)
        carried? (> result (:a cpu))
        overflowed? (subtract-overflowed? (:a cpu) arg result)
        updates {carry-flag carried?
                 overflow-flag overflowed?
                 negative-flag (negative? result)
                 zero-flag (zero? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:a result :p new-flags})))

(defn and*
  [cpu arg]
  (let [result (unsigned-byte (bit-and (:a cpu) arg))
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
  (merge cpu {:a result :p new-flags})))

(defn ora
  [cpu arg]
  (let [result (unsigned-byte (bit-or (:a cpu) arg))
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:a result :p new-flags})))
