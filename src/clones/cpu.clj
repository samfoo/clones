(ns clones.cpu)

(defn bit-set? [x mask] (= (bit-and x mask) mask))

(defn make-cpu
  []
  { :a (byte 0x00)
    :x (byte 0x00)
    :y (byte 0x00)
    :sp (byte 0x00)
    :p (byte 0x00)
    :pc 0x0000 })

(defn update-flags [cpu new-flags] (assoc cpu :p new-flags))

(defn set-flag [flags mask] (bit-or flags mask))
(defn unset-flag [flags mask] (bit-and flags (bit-not mask)))
(defn flag? [flags mask] (bit-set? flags mask))

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

(defn add-overflowed?
  [orig arg result]
  (let [
        orig-neg? (bit-set? orig 0x80)
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
  (let [result (if (carry-flag? cpu)
                 (+ (unchecked-byte (:a cpu)) (unchecked-byte arg) 1)
                 (+ (unchecked-byte (:a cpu)) (unchecked-byte arg)))
        flags (:p cpu)
        carried? (< result (:a cpu))
        overflowed? (add-overflowed? (:a cpu) arg result)
        updates {
                 carry-flag carried?
                 overflow-flag overflowed?
                 negative-flag (negative? result)
                 zero-flag (zero? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:a result :p new-flags})))

