(ns clones.cpu)

(defn unsigned-byte [b] (bit-and 0xff b))
(defn bit-set? [x mask] (= (bit-and x mask) mask))

(defn make-cpu [] {:a 0
                   :x 0
                   :y 0
                   :sp (unsigned-byte 0xfd)
                   :p 0
                   :pc 0})

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

(defmacro defasm [op-name action]
  (let [fn-args (vector 'cpu 'arg)]
    `(defn ~(symbol (str "*asm-" (name op-name)))
       ~fn-args
       ~action)))

;; Comparison operations
(defn compare-op
  [cpu arg reg]
  (let [result (unsigned-byte (- (reg cpu) arg))
        register (unsigned-byte (reg cpu))
        value (unsigned-byte arg)
        flags (:p cpu)
        updates {carry-flag (>= register value)
                 negative-flag (negative? result)
                 zero-flag (zero? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:p new-flags})))

(defasm cmp (compare-op cpu arg :a))
(defasm cpx (compare-op cpu arg :x))
(defasm cpy (compare-op cpu arg :y))

;; Arithmetic operations
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

(defasm adc
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

(defasm sbc
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

;; Logical operations
(defn logical-op
  [cpu arg method]
  (let [result (unsigned-byte (method (:a cpu) arg))
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:a result :p new-flags})))

(defasm and (logical-op cpu arg bit-and))
(defasm ora (logical-op cpu arg bit-or))
(defasm eor (logical-op cpu arg bit-xor))

(defasm bit
  (let [result (unsigned-byte (bit-and (:a cpu) arg))
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 overflow-flag (== 0x40 (bit-and result 0x40))
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
    (merge cpu {:p new-flags})))

;; Load operations
(defn load-op
  [cpu arg reg]
  (let [result (unsigned-byte arg)
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
  (merge cpu {reg result :p new-flags})))

(defasm lda (load-op cpu arg :a))
(defasm ldx (load-op cpu arg :x))
(defasm ldy (load-op cpu arg :y))

;; Register transfers
(defn transfer-reg-op
  [cpu from to]
  (let [result (from cpu)
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
  (merge cpu {to result :p new-flags})))

(defasm tax (transfer-reg-op cpu :a :x))
(defasm tay (transfer-reg-op cpu :a :y))
(defasm txa (transfer-reg-op cpu :x :a))
(defasm tya (transfer-reg-op cpu :y :a))
(defasm tsx (transfer-reg-op cpu :sp :x))
(defasm txs (transfer-reg-op cpu :x :sp))

;; Increment & decrements
(defn increment-op
  [cpu reg]
  (let [result (unsigned-byte (+ (reg cpu) 1))
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
  (merge cpu {reg result :p new-flags})))

(defasm inc (increment-op cpu :a))
(defasm inx (increment-op cpu :x))
(defasm iny (increment-op cpu :y))

(defn decrement-op
  [cpu reg]
  (let [result (unsigned-byte (- (reg cpu) 1))
        flags (:p cpu)
        updates {zero-flag (zero? result)
                 negative-flag (negative? result)}
        new-flags (set-flags flags updates)]
  (merge cpu {reg result :p new-flags})))

(defasm dec (decrement-op cpu :a))
(defasm dex (decrement-op cpu :x))
(defasm dey (decrement-op cpu :y))
