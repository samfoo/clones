(ns clones.cpu
  (:require [clones.memory :refer :all]
            [clones.byte   :refer :all]))

(defn make-cpu []
  (let [state {:a 0
               :x 0
               :y 0
               :sp (unsigned-byte 0xfd)
               :p 0
               :pc 0}]
    (assoc state :memory (mount-device [] 0 0x1fff {}))))

(defn negative? [b] (== 0x80 (bit-and b 0x80)))

(def carry-flag 0x01)
(def zero-flag 0x02)
(def interrupt-flag 0x04)
(def decimal-flag 0x08)
(def break-flag 0x10)
(def unused-flag 0x20)
(def overflow-flag 0x40)
(def negative-flag 0x80)

(defn flag? [flags mask] (bit-set? flags mask))
(defn carry-flag? [cpu] (flag? (:p cpu) carry-flag))
(defn zero-flag? [cpu] (flag? (:p cpu) zero-flag))
(defn overflow-flag? [cpu] (flag? (:p cpu) overflow-flag))
(defn negative-flag? [cpu] (flag? (:p cpu) negative-flag))

(defn set-flag [cpu flag v]
  (let [flags (:p cpu)]
    (if v
      (assoc cpu :p (bit-or flags flag))
      (assoc cpu :p (bit-and flags (bit-not flag))))))

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
        value (unsigned-byte arg)]
    (-> cpu
      (set-flag carry-flag (>= register value))
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result)))))

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
        carried? (< result (:a cpu))
        overflowed? (add-overflowed? (:a cpu) arg result)]
    (-> cpu
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (assoc :a result))))

(defasm sbc
  (let [result (unsigned-byte (if (carry-flag? cpu)
                 (- (:a cpu) arg)
                 (- (:a cpu) arg 1)))
        carried? (> result (:a cpu))
        overflowed? (subtract-overflowed? (:a cpu) arg result)]
    (-> cpu
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (assoc :a result))))

;; Logical operations
(defn logical-op
  [cpu arg method]
  (let [result (unsigned-byte (method (:a cpu) arg))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc :a result))))

(defasm and (logical-op cpu arg bit-and))
(defasm ora (logical-op cpu arg bit-or))
(defasm eor (logical-op cpu arg bit-xor))

(defasm bit
  (let [result (unsigned-byte (bit-and (:a cpu) arg))
        overflowed? (== 0x40 (bit-and result 0x40))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result)))))

;; Load operations
(defn load-op
  [cpu arg reg]
  (let [result (unsigned-byte arg)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defasm lda (load-op cpu arg :a))
(defasm ldx (load-op cpu arg :x))
(defasm ldy (load-op cpu arg :y))

;; Register transfers
(defn transfer-reg-op
  [cpu from to]
  (let [result (from cpu)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc to result))))

(defasm tax (transfer-reg-op cpu :a :x))
(defasm tay (transfer-reg-op cpu :a :y))
(defasm txa (transfer-reg-op cpu :x :a))
(defasm tya (transfer-reg-op cpu :y :a))
(defasm tsx (transfer-reg-op cpu :sp :x))
(defasm txs (transfer-reg-op cpu :x :sp))

;; Increment & decrements
(defn increment-op
  [cpu reg]
  (let [result (unsigned-byte (inc (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defasm inc (increment-op cpu :a))
(defasm inx (increment-op cpu :x))
(defasm iny (increment-op cpu :y))

(defn decrement-op
  [cpu reg]
  (let [result (unsigned-byte (dec (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defasm dec (decrement-op cpu :a))
(defasm dex (decrement-op cpu :x))
(defasm dey (decrement-op cpu :y))

;; Stack pushing and popping
(defn push [cpu v]
  (let [mem (mount-write (:memory cpu) v (+ 0x100 (:sp cpu)))
        new-sp (unsigned-byte (dec (:sp cpu)))]
    (merge cpu {:memory mem :sp new-sp})))

(defasm pha (push cpu (:a cpu)))
(defasm php (push cpu (bit-or 0x10 (:p cpu))))

(defn pull [cpu reg]
  (let [v (mount-read (:memory cpu) (+ 0x100 1 (:sp cpu)))]
    (merge cpu {reg v :sp (inc (:sp cpu))})))

(defn pull-pc [cpu]
  (let [stack (:memory cpu)
        low (mount-read stack (+ 0x100 1 (:sp cpu)))
        high (bit-shift-left (mount-read stack (+ 0x100 2 (:sp cpu))) 8)
        new-pc (bit-or low high)]
    (merge cpu {:pc new-pc :sp (+ 2 (:sp cpu))})))

(defn pull-flags [cpu]
  (let [pulled (pull cpu :p)]
    (-> pulled
      (set-flag break-flag false)
      (set-flag unused-flag true))))

(defasm pla
  (let [pulled (pull cpu :a)
        result (:a pulled)]
    (-> pulled
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defasm plp (pull-flags cpu))

;; Jumps and calls
(defasm jmp (assoc cpu :pc arg))

(defasm jsr
  (let [pc (dec (:pc cpu))
        high (high-byte pc)
        low  (low-byte pc)]
    (-> cpu
      (push high)
      (push low)
      (assoc :pc arg))))

(defasm rti
  (-> cpu
    (pull-flags)
    (pull-pc)))

(defasm rts
  (let [pulled (pull-pc cpu)]
    (assoc pulled :pc (inc (:pc pulled)))))
