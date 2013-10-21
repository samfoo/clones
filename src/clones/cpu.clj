(ns clones.cpu
  (:require [clones.memory     :refer :all]
            [clones.byte       :refer :all]
            [clones.addressing :refer :all]))

(def operations {})

(defmacro defop [op-name opcodes action]
  (let [fn-args ['cpu '&
                 {:keys ['operand 'address-mode]
                  :or {'operand nil
                       'address-mode nil}}]
        fn-name (symbol (str "*" (name op-name)))]
    `(let [~'op-fn (defn ~fn-name
                   ~fn-args
                   ~action)]
      (def operations
         (reduce (fn [~'m ~'op]
                   (assoc ~'m (first ~'op) {:address-mode (second ~'op)
                                            :op ~'op-fn}))
                 operations
                 (partition 2 ~opcodes))))))

(defn make-cpu []
  (let [state {:a 0
               :x 0
               :y 0
               :sp (unsigned-byte 0xfd)
               :p 0
               :pc 0}
        memory (-> []
                 (mount-device 0      0x1fff {})   ;; 8kb of internal ram.
                 (mount-device 0xfffa 0xffff {}))] ;; Interrupt vectors.
    (assoc state :memory memory)))

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

(defop cmp [0xc9 immediate
            0xc5 zero-page
            0xd5 zero-page-x
            0xcd absolute
            0xdd absolute-x
            0xd9 absolute-y
            0xc1 indexed-indirect
            0xd1 indirect-indexed]
  (compare-op cpu operand :a))

(defop cpx [0xe0 immediate
            0xe4 zero-page
            0xec absolute]
  (compare-op cpu operand :x))

(defop cpy [0xc0 immediate
            0xc4 zero-page
            0xcc absolute]
  (compare-op cpu operand :y))

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

(defop adc [0x69 immediate
            0x65 zero-page
            0x75 zero-page-x
            0x6d absolute
            0x7d absolute-x
            0x79 absolute-y
            0x61 indexed-indirect
            0x71 indirect-indexed]
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

(defop sbc [0xe9 immediate
            0xe5 zero-page
            0xf5 zero-page-x
            0xed absolute
            0xfd absolute-x
            0xf9 absolute-y
            0xe1 indexed-indirect
            0xf1 indirect-indexed]
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

(defop and [0x29 immediate
            0x25 zero-page
            0x45 zero-page-x
            0x2d absolute
            0x3d absolute-x
            0x39 absolute-y
            0x21 indexed-indirect
            0x31 indirect-indexed]
  (logical-op cpu operand bit-and))

(defop ora [0x09 immediate
            0x05 zero-page
            0x15 zero-page-x
            0x0d absolute
            0x1d absolute-x
            0x19 absolute-y
            0x01 indexed-indirect
            0x11 indirect-indexed]
  (logical-op cpu operand bit-or))

(defop eor [0x49 immediate
            0x45 zero-page
            0x55 zero-page-x
            0x4d absolute
            0x5d absolute-x
            0x59 absolute-y
            0x41 indexed-indirect
            0x51 indirect-indexed]
  (logical-op cpu operand bit-xor))

(defop bit [0x24 zero-page
            0x2c absolute]
  (let [result (unsigned-byte (bit-and (:a cpu) operand))
        overflowed? (== 0x40 (bit-and result 0x40))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result)))))

;; Load & store operations
(defn load-op
  [cpu operand reg]
  (let [result (unsigned-byte operand)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop lda [0xa9 immediate
            0xa5 zero-page
            0xb5 zero-page-x
            0xad absolute
            0xbd absolute-x
            0xb9 absolute-y
            0xa1 indexed-indirect
            0xb1 indirect-indexed]
  (load-op cpu operand :a))

(defop ldx [0xa2 immediate
            0xa6 zero-page
            0xb6 zero-page-x
            0xae absolute
            0xbe absolute-x]
  (load-op cpu operand :x))

(defop ldy [0xa0 immediate
            0xa4 zero-page
            0xb4 zero-page-x
            0xac absolute
            0xbc absolute-x]
  (load-op cpu operand :y))

(defn store-op
  [cpu address-mode reg]
  (second (io-> cpu
                (mode-write address-mode (reg cpu)))))

(defop sta [0x85 zero-page
            0x95 zero-page-x
            0x8d absolute
            0x9d absolute-x
            0x99 absolute-y
            0x81 indexed-indirect
            0x91 indirect-indexed]
  (store-op cpu address-mode :a))

(defop stx [0x86 zero-page
            0x96 zero-page-y
            0x8e absolute]
  (store-op cpu address-mode :x))

(defop sty [0x84 zero-page
            0x94 zero-page-x
            0x8c absolute]
  (store-op cpu address-mode :y))

;; Register transfers
(defn transfer-reg-op
  [cpu from to]
  (let [result (from cpu)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc to result))))

(defop tax [0xaa implied] (transfer-reg-op cpu :a :x))
(defop tay [0xa8 implied] (transfer-reg-op cpu :a :y))
(defop txa [0x8a implied] (transfer-reg-op cpu :x :a))
(defop tya [0x98 implied] (transfer-reg-op cpu :y :a))
(defop tsx [0x9a implied] (transfer-reg-op cpu :sp :x))
(defop txs [0xba implied] (transfer-reg-op cpu :x :sp))

;; Increment & decrements
(defn increment-op
  [cpu reg]
  (let [result (unsigned-byte (inc (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop inc [0xe6 zero-page
            0xf6 zero-page-x
            0xee absolute
            0xfe absolute-x]
   (let [[result after-io] ((with-io-> [orig (mode-read address-mode)
                                        incd (let [result (unsigned-byte (inc orig))]
                                               (mode-write address-mode result))]
                                       incd) cpu)]
     (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop inx [0xe8 implied] (increment-op cpu :x))
(defop iny [0xc8 implied] (increment-op cpu :y))

(defn dec-reg-op
  [cpu reg]
  (let [result (unsigned-byte (dec (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop dec [0xc6 zero-page
            0xd6 zero-page-x
            0xce absolute
            0xde absolute-x]
  (let [[result after-io] ((with-io-> [before (mode-read address-mode)
                                       after (mode-write address-mode
                                               (unsigned-byte
                                                 (dec before)))]
                                      after) cpu)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop dex [0xca implied] (dec-reg-op cpu :x))
(defop dey [0x88 implied] (dec-reg-op cpu :y))

;; Stack pushing and popping
(defn stack-top [cpu] (+ 0x100 (:sp cpu)))

(defn stack-next [top]
  (unsigned-byte (+ 1 top)))

(defn stack-push [cpu v]
  (let [top (stack-top cpu)
        [_ after-push] (io-> cpu
                             (io-write v top))]
    (assoc after-push :sp (unsigned-byte (dec top)))))

(defop pha [0x48 implied] (stack-push cpu (:a cpu)))
(defop php [0x08 implied] (stack-push cpu (bit-or 0x10 (:p cpu))))

(defn stack-pull [cpu reg]
  (let [top (stack-top cpu)
        [v after-pull] (io-> cpu
                             (io-read (+ 1 (stack-top cpu))))]
    (merge after-pull {reg v :sp (stack-next top)})))

(defn stack-pull-pc [cpu]
  (let [top (stack-top cpu)
        [v after-pull] (io-> cpu
                             (io-read-word (+ 1 (stack-top cpu))))]
    (merge after-pull {:pc v :sp (stack-next (stack-next top))})))

(defn stack-pull-flags [cpu]
  (let [pulled (stack-pull cpu :p)]
    (-> pulled
      (set-flag break-flag false)
      (set-flag unused-flag true))))

(defn interrupt-vector [cpu]
  (io-> cpu (io-read-word 0xfffe)))

(defop pla [0x68 implied]
  (let [pulled (stack-pull cpu :a)
        result (:a pulled)]
    (-> pulled
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop plp [0x28 implied] (stack-pull-flags cpu))

;; Jumps and calls
(defop jmp [0x4c absolute
            0x6c indirect]
  (assoc cpu :pc operand))

(defop jsr [0x20 absolute]
  (let [pc (dec (:pc cpu))
        high (high-byte pc)
        low  (low-byte pc)]
    (-> cpu
      (stack-push high)
      (stack-push low)
      (assoc :pc operand))))

(defop rti [0x40 implied]
  (-> cpu
    (stack-pull-flags)
    (stack-pull-pc)))

(defop rts [0x60 implied]
  (let [pulled (stack-pull-pc cpu)]
    (assoc pulled :pc (inc (:pc pulled)))))

;; Branching
(defn branch-if [cpu predicate addr]
  (if predicate
    (assoc cpu :pc addr)
    cpu))

(defop bcc [0x90 relative] (branch-if cpu (not (carry-flag? cpu)) operand))
(defop bcs [0xb0 relative] (branch-if cpu (carry-flag? cpu) operand))
(defop beq [0xf0 relative] (branch-if cpu (zero-flag? cpu) operand))
(defop bmi [0x30 relative] (branch-if cpu (negative-flag? cpu) operand))
(defop bne [0xd0 relative] (branch-if cpu (not (zero-flag? cpu)) operand))
(defop bpl [0x10 relative] (branch-if cpu (not (negative-flag? cpu)) operand))
(defop bvc [0x50 relative] (branch-if cpu (not (overflow-flag? cpu)) operand))
(defop bvs [0x70 relative] (branch-if cpu (overflow-flag? cpu) operand))

;; Status flag changes
(defop clc [0x18 implied] (set-flag cpu carry-flag false))
(defop cld [0xd8 implied] (set-flag cpu decimal-flag false))
(defop cli [0x58 implied] (set-flag cpu interrupt-flag false))
(defop clv [0xb8 implied] (set-flag cpu overflow-flag false))
(defop sec [0x38 implied] (set-flag cpu carry-flag true))
(defop sed [0xf8 implied] (set-flag cpu decimal-flag true))
(defop sei [0x78 implied] (set-flag cpu interrupt-flag true))

;; System functions
(defop nop [0xea implied] cpu)

(defop brk [0x00 implied]
  (let [pc (inc (:pc cpu))
        [interrupt after-read] (interrupt-vector cpu)
        high (high-byte pc)
        low  (low-byte pc)]
    (-> after-read
      (stack-push high)
      (stack-push low)
      (stack-push (bit-or 0x10 (:p after-read)))
      (assoc :pc interrupt))))

;; Shifts

(defop asl [0x0a accumulator
            0x06 zero-page
            0x16 zero-page-x
            0x0e absolute
            0x1d absolute-x]
  (let [[[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                       (unsigned-byte
                                                         (bit-shift-left before 1)))]
                                             [before after]) cpu)
        carried? (bit-set? orig 0x80)
        negative? (bit-set? result 0x80)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag negative?)
      (set-flag carry-flag carried?))))

(defop lsr [0x4a accumulator
            0x46 zero-page
            0x56 zero-page-x
            0x4e absolute
            0x5e absolute-x]
  (let [[[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                      (unsigned-byte
                                                        (bit-shift-right before 1)))]
                                             [before after]) cpu)
        carried? (bit-set? orig 1)]
    (-> after-io
      (set-flag carry-flag carried?)
      (set-flag negative-flag false)
      (set-flag zero-flag (zero? result)))))

(defn rotate-l [v]
  (let [shifted (unsigned-byte (bit-shift-left v 1))]
    (if (bit-set? v 0x80)
      (bit-or 1 shifted)
      shifted)))

(defop rol [0x2a accumulator
            0x26 zero-page
            0x36 zero-page-x
            0x2e absolute
            0x3d absolute-x]
  (let [[[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                      (rotate-l before))]
                                             [before after]) cpu)
        carried? (bit-set? orig 0x80)
        negative? (bit-set? result 0x80)]
    (-> after-io
      (set-flag negative-flag negative?)
      (set-flag zero-flag (zero? result))
      (set-flag carry-flag carried?))))

(defn rotate-r [v]
  (let [shifted (unsigned-byte (bit-shift-right v 1))]
    (if (bit-set? v 1)
      (bit-or 0x80 shifted)
      shifted)))


(defop ror [0x6a accumulator
            0x66 zero-page
            0x76 zero-page-x
            0x6e absolute
            0x7e absolute-x]
  (let [[[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                      (rotate-r before))]
                                             [before after]) cpu)
        carried? (bit-set? orig 1)
        negative? (bit-set? result 0x80)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag negative?)
      (set-flag carry-flag carried?))))
