(ns clones.cpu
  (:require [clones.memory :refer :all]
            [clones.byte   :refer :all]))

(defmacro defop [op-name action]
  (let [fn-args (vector 'cpu '& {:keys ['operand] :or {'operand nil}})]
    `(defn ~(symbol (str "*" (name op-name)))
       ~fn-args
       ~action)))

(defn make-cpu []
  (let [state {:a 0
               :x 0
               :y 0
               :sp (unsigned-byte 0xfd)
               :p 0
               :pc 0}
        memory (-> []
                 (mount-device 0      0x1fff {})
                 (mount-device 0xfffa 0xffff {}))]
    (assoc state :memory memory)))

(defn cpu-write [cpu v addr]
  (assoc cpu :memory (mount-write (:memory cpu)
                                  v
                                  addr)))

(defn cpu-read [cpu addr]
  (mount-read (:memory cpu) addr))

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
(defn decimal-flag? [cpu] (flag? (:p cpu) decimal-flag))
(defn interrupt-flag? [cpu] (flag? (:p cpu) interrupt-flag))
(defn overflow-flag? [cpu] (flag? (:p cpu) overflow-flag))
(defn negative-flag? [cpu] (flag? (:p cpu) negative-flag))

(defn set-flag [cpu flag v]
  (let [flags (:p cpu)]
    (if v
      (assoc cpu :p (bit-or flags flag))
      (assoc cpu :p (bit-and flags (bit-not flag))))))

;; Comparison operations
(defn compare-op
  [cpu operand reg]
  (let [result (unsigned-byte (- (reg cpu) operand))
        register (unsigned-byte (reg cpu))
        value (unsigned-byte operand)]
    (-> cpu
      (set-flag carry-flag (>= register value))
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result)))))

(defop cmp (compare-op cpu operand :a))
(defop cpx (compare-op cpu operand :x))
(defop cpy (compare-op cpu operand :y))

;; Arithmetic operations
(defn subtract-overflowed?
  [orig operand result]
  (let [orig-neg? (bit-set? orig 0x80)
        operand-neg? (bit-set? operand 0x80)
        result-neg? (bit-set? result 0x80)]
    (if (and (not orig-neg?) operand-neg? result-neg?)
      ;; Subtracting a negative from a positive shouldn't result in a negative
      true
      (if (and orig-neg? (not operand-neg?) (not result-neg?))
        ;; Subtracking a positive from a negative shouldn't result in a
        ;; positive
        true
        false))))

(defn add-overflowed?
  [orig operand result]
  (let [orig-neg? (bit-set? orig 0x80)
        operand-neg? (bit-set? operand 0x80)
        result-neg? (bit-set? result 0x80)]
    (if (and (not orig-neg?) (not operand-neg?) result-neg?)
      ;; Adding two positives should not result in a negative
      true
      (if (and orig-neg? operand-neg? (not result-neg?))
        ;; Adding two negatives should not result in a positive
        true
        false))))

(defop adc
  (let [result (unsigned-byte (if (carry-flag? cpu)
                 (+ (:a cpu) operand 1)
                 (+ (:a cpu) operand)))
        carried? (< result (:a cpu))
        overflowed? (add-overflowed? (:a cpu) operand result)]
    (-> cpu
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (assoc :a result))))

(defop sbc
  (let [result (unsigned-byte (if (carry-flag? cpu)
                 (- (:a cpu) operand)
                 (- (:a cpu) operand 1)))
        carried? (> result (:a cpu))
        overflowed? (subtract-overflowed? (:a cpu) operand result)]
    (-> cpu
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (assoc :a result))))

;; Logical operations
(defn logical-op
  [cpu operand method]
  (let [result (unsigned-byte (method (:a cpu) operand))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc :a result))))

(defop and (logical-op cpu operand bit-and))
(defop ora (logical-op cpu operand bit-or))
(defop eor (logical-op cpu operand bit-xor))

(defop bit
  (let [result (unsigned-byte (bit-and (:a cpu) operand))
        overflowed? (== 0x40 (bit-and result 0x40))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result)))))

;; Load operations
(defn load-op
  [cpu operand reg]
  (let [result (unsigned-byte operand)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop lda (load-op cpu operand :a))
(defop ldx (load-op cpu operand :x))
(defop ldy (load-op cpu operand :y))

;; Register transfers
(defn transfer-reg-op
  [cpu from to]
  (let [result (from cpu)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc to result))))

(defop tax (transfer-reg-op cpu :a :x))
(defop tay (transfer-reg-op cpu :a :y))
(defop txa (transfer-reg-op cpu :x :a))
(defop tya (transfer-reg-op cpu :y :a))
(defop tsx (transfer-reg-op cpu :sp :x))
(defop txs (transfer-reg-op cpu :x :sp))

;; Increment & decrements
(defn increment-op
  [cpu reg]
  (let [result (unsigned-byte (inc (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop inc (increment-op cpu :a))
(defop inx (increment-op cpu :x))
(defop iny (increment-op cpu :y))

(defn decrement-op
  [cpu reg]
  (let [result (unsigned-byte (dec (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop dec (decrement-op cpu :a))
(defop dex (decrement-op cpu :x))
(defop dey (decrement-op cpu :y))

;; Stack pushing and popping
(defn push [cpu v]
  (-> cpu
    (cpu-write v (+ 0x100 (:sp cpu)))
    (assoc :sp (unsigned-byte (dec (:sp cpu))))))

(defop pha (push cpu (:a cpu)))
(defop php (push cpu (bit-or 0x10 (:p cpu))))

(defn pull [cpu reg]
  (let [v (cpu-read cpu (+ 0x100 1 (:sp cpu)))]
    (merge cpu {reg v :sp (inc (:sp cpu))})))

(defn pull-pc [cpu]
  (let [low (cpu-read cpu (+ 0x100 1 (:sp cpu)))
        high (bit-shift-left (cpu-read cpu (+ 0x100 2 (:sp cpu))) 8)
        new-pc (bit-or low high)]
    (merge cpu {:pc new-pc :sp (+ 2 (:sp cpu))})))

(defn pull-flags [cpu]
  (let [pulled (pull cpu :p)]
    (-> pulled
      (set-flag break-flag false)
      (set-flag unused-flag true))))

(defn interrupt-vector [cpu]
  (let [high (bit-shift-left (cpu-read cpu 0xffff) 8)
        low (cpu-read cpu 0xfffe)]
    (bit-or high low)))

(defop pla
  (let [pulled (pull cpu :a)
        result (:a pulled)]
    (-> pulled
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop plp (pull-flags cpu))

;; Jumps and calls
(defop jmp (assoc cpu :pc operand))

(defop jsr
  (let [pc (dec (:pc cpu))
        high (high-byte pc)
        low  (low-byte pc)]
    (-> cpu
      (push high)
      (push low)
      (assoc :pc operand))))

(defop rti
  (-> cpu
    (pull-flags)
    (pull-pc)))

(defop rts
  (let [pulled (pull-pc cpu)]
    (assoc pulled :pc (inc (:pc pulled)))))

;; Branching
(defn branch-if [cpu predicate addr]
  (if predicate
    (assoc cpu :pc addr)
    cpu))

(defop bcc (branch-if cpu (not (carry-flag? cpu)) operand))
(defop bcs (branch-if cpu (carry-flag? cpu) operand))
(defop beq (branch-if cpu (zero-flag? cpu) operand))
(defop bmi (branch-if cpu (negative-flag? cpu) operand))
(defop bne (branch-if cpu (not (zero-flag? cpu)) operand))
(defop bpl (branch-if cpu (not (negative-flag? cpu)) operand))
(defop bvc (branch-if cpu (not (overflow-flag? cpu)) operand))
(defop bvs (branch-if cpu (overflow-flag? cpu) operand))

;; Status flag changes
(defop clc (set-flag cpu carry-flag false))
(defop cld (set-flag cpu decimal-flag false))
(defop cli (set-flag cpu interrupt-flag false))
(defop clv (set-flag cpu overflow-flag false))
(defop sec (set-flag cpu carry-flag true))
(defop sed (set-flag cpu decimal-flag true))
(defop sei (set-flag cpu interrupt-flag true))

;; System functions
(defop nop cpu)

(defop brk
  (let [pc (inc (:pc cpu))
        interrupt (interrupt-vector cpu)
        high (high-byte pc)
        low  (low-byte pc)]
    (-> cpu
      (push high)
      (push low)
      (push (bit-or 0x10 (:p cpu)))
      (assoc :pc interrupt))))
