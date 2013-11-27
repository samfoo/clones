(ns clones.cpu
  (:require [clones.cpu.memory     :refer :all]
            [clones.cpu.addressing :refer :all]
            [clones.byte           :refer :all]))

(def op-codes {})
(def ops {})

(defn op [n] (n ops))

(defrecord CPU [^int a
                ^int x
                ^int y
                ^int sp
                ^int p
                ^int pc
                memory])

(defmacro defop [op-name opcodes action]
  (let [fn-args [^CPU 'cpu 'address-mode]]
    `(let [~'op-fn (fn ~fn-args
                     (let [~'cpu (with-meta ~'cpu {})]
                       ~action))]
       (def ops (assoc ops (keyword '~op-name) ~'op-fn))
       (def op-codes
         (reduce (fn [~'m ~'op]
                   (assoc ~'m
                          (first ~'op)
                          (with-meta ~'op-fn {:address-mode (second ~'op)
                                              :cycles (nth ~'op 2)
                                              :name (name '~op-name)})))
                 op-codes
                 (partition 3 ~opcodes))))))

(defn make-cpu [bus] (CPU. 0 0 0 0xfd 0x24 0 bus))

(defn- inc-pc [cpu]
  (assoc cpu :pc (inc (:pc cpu))))

(defn execute-with-timing [cpu op]
  (let [{:keys [address-mode name cycles]} (meta op)
        after-op (op cpu address-mode)
        cs (cycles cpu after-op address-mode)]
    [cs after-op]))

(defn- advance-pc [cpu mode]
  (assoc cpu :pc (+
                   (:pc cpu)
                   (mode-size mode))))

(defn cpu-step [machine]
  (let [cpu (:cpu machine)
        [op-code after-read] (io-> cpu (io-read (:pc cpu)))
        op (get op-codes op-code)]
    (if (nil? op)
      (throw (ex-info (format "Invalid op code $%02x" op-code) {:op-code op-code}))
      nil)
    (let [[cs after-op] (execute-with-timing (inc-pc after-read) op)]
      [cs (assoc machine :cpu after-op)])))

(defn negative? [b] (== 0x80 (bit-and b 0x80)))

(def carry-flag 0x01)
(def zero-flag 0x02)
(def interrupt-flag 0x04)
(def decimal-flag 0x08)
(def break-flag 0x10)
(def unused-flag 0x20)
(def overflow-flag 0x40)
(def negative-flag 0x80)

(defn flag? [flags mask] (= mask (bit-and flags mask)))
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

(defn- different-pages? [a1 a2]
  (not= (bit-and 0xff00 a1) (bit-and 0xff00 a2)))

(defn- cycles-page-crossed-penalty-abs-reg [cpu address-mode penalty]
  (let [abs-addr (first (io-> cpu (absolute)))
        abs-addr-w-reg (first (io-> cpu (address-mode)))]
    (if (different-pages? abs-addr abs-addr-w-reg)
      penalty
      0)))

(defn- cycles-page-crossed-penalty-ind-idx [cpu address-mode penalty]
  (let [i-addr (first (io-> cpu (indirect-indexed-addr)))
        i-addr-w-reg (first (io-> cpu (indirect-indexed)))]
    (if (different-pages? i-addr i-addr-w-reg)
      penalty
      0)))

(defn- cycles-page-crossed-penalty [cpu address-mode penalty]
  (if (= indirect-indexed address-mode)
    (cycles-page-crossed-penalty-ind-idx cpu address-mode penalty)
    (cycles-page-crossed-penalty-abs-reg cpu address-mode penalty)))

(defn- cycles [base & r]
  (fn [before-cpu after-cpu address-mode]
    (let [opts (apply hash-map r)
          page-cross-penalty (if (contains? opts :cross-page)
                               (cycles-page-crossed-penalty before-cpu address-mode (:cross-page opts))
                               0)]
      (+ base page-cross-penalty))))

(defn- cycles-branched-instr [before-cpu after-cpu c]
  (let [pc-after-branch-instr (unsigned-word (+ 2 (:pc before-cpu)))]
    (if (different-pages? pc-after-branch-instr (:pc after-cpu))
      (+ c 2)
      (+ c 1))))

(defn- branched? [cpu]
  (get (meta cpu) :branched? false))

(defn- branch-cycles [base]
  (fn [before-cpu after-cpu address-mode]
    (if (branched? after-cpu)
      (cycles-branched-instr before-cpu after-cpu base)
      base)))

;; Comparison operations
(defn compare-op
  [cpu reg operand]
  (let [result (unsigned-byte (- (reg cpu) operand))
        value (unsigned-byte operand)]
  (-> cpu
    (set-flag carry-flag (>= (reg cpu) value))
    (set-flag negative-flag (negative? result))
    (set-flag zero-flag (zero? result)))))

(defn compare-op-with-io
  [cpu mode reg]
  (let [[operand after-io] (io-> cpu (mode-read mode))]
    (-> after-io
      (compare-op reg operand)
      (advance-pc mode))))

(defop cmp [0xc9 immediate        (cycles 2)
            0xc5 zero-page        (cycles 3)
            0xd5 zero-page-x      (cycles 4)
            0xcd absolute         (cycles 4)
            0xdd absolute-x       (cycles 4 :cross-page 1)
            0xd9 absolute-y       (cycles 4 :cross-page 1)
            0xc1 indexed-indirect (cycles 6)
            0xd1 indirect-indexed (cycles 5 :cross-page 1)]
  (compare-op-with-io cpu address-mode :a))

(defop cpx [0xe0 immediate (cycles 2)
            0xe4 zero-page (cycles 3)
            0xec absolute  (cycles 4)]
  (compare-op-with-io cpu address-mode :x))

(defop cpy [0xc0 immediate (cycles 2)
            0xc4 zero-page (cycles 3)
            0xcc absolute  (cycles 4)]
  (compare-op-with-io cpu address-mode :y))

;; Arithmetic operations
(defn subtract-overflowed?
  [orig operand result]
  (let [orig-neg? (bit-set? orig 7)
        operand-neg? (bit-set? operand 7)
        result-neg? (bit-set? result 7)]
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
  (let [orig-neg? (bit-set? orig 7)
        operand-neg? (bit-set? operand 7)
        result-neg? (bit-set? result 7)]
    (if (and (not orig-neg?) (not operand-neg?) result-neg?)
      ;; Adding two positives should not result in a negative
      true
      (if (and orig-neg? operand-neg? (not result-neg?))
        ;; Adding two negatives should not result in a positive
        true
        false))))

(defn add-op [cpu operand]
  (let [signed-result (if (carry-flag? cpu)
                        (+ (:a cpu) operand 1)
                        (+ (:a cpu) operand))
        result (unsigned-byte signed-result)
        carried? (> signed-result 0xff)
        overflowed? (add-overflowed? (:a cpu) operand result)]
    (-> cpu
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (assoc :a result))))

(defop adc [0x69 immediate        (cycles 2)
            0x65 zero-page        (cycles 3)
            0x75 zero-page-x      (cycles 4)
            0x6d absolute         (cycles 4)
            0x7d absolute-x       (cycles 4 :cross-page 1)
            0x79 absolute-y       (cycles 4 :cross-page 1)
            0x61 indexed-indirect (cycles 6)
            0x71 indirect-indexed (cycles 5 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (-> after-io
      (add-op operand)
      (advance-pc address-mode))))

(defn subtract-op [cpu operand]
  (let [signed-result (if (carry-flag? cpu)
                        (- (:a cpu) operand)
                        (- (:a cpu) operand 1))
        result (unsigned-byte signed-result)
        carried? (>= signed-result 0)
        overflowed? (subtract-overflowed? (:a cpu) operand result)]
    (-> cpu
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (assoc :a result))))

(defop sbc [0xe9 immediate        (cycles 2)
            0xe5 zero-page        (cycles 3)
            0xf5 zero-page-x      (cycles 4)
            0xed absolute         (cycles 4)
            0xfd absolute-x       (cycles 4 :cross-page 1)
            0xf9 absolute-y       (cycles 4 :cross-page 1)
            0xe1 indexed-indirect (cycles 6)
            0xf1 indirect-indexed (cycles 5 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (-> after-io
      (subtract-op operand)
      (advance-pc address-mode))))

;; Logical operations
(defn logical-op
  [cpu mode operand method]
  (let [result (unsigned-byte (method (:a cpu) operand))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc :a result)
      (advance-pc mode))))

(defop and [0x29 immediate        (cycles 2)
            0x25 zero-page        (cycles 3)
            0x35 zero-page-x      (cycles 4)
            0x2d absolute         (cycles 4)
            0x3d absolute-x       (cycles 4 :cross-page 1)
            0x39 absolute-y       (cycles 4 :cross-page 1)
            0x21 indexed-indirect (cycles 6)
            0x31 indirect-indexed (cycles 5 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (logical-op after-io address-mode operand bit-and)))

(defop ora [0x09 immediate        (cycles 2)
            0x05 zero-page        (cycles 3)
            0x15 zero-page-x      (cycles 4)
            0x0d absolute         (cycles 4)
            0x1d absolute-x       (cycles 4 :cross-page 1)
            0x19 absolute-y       (cycles 4 :cross-page 1)
            0x01 indexed-indirect (cycles 6)
            0x11 indirect-indexed (cycles 5 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (logical-op after-io address-mode operand bit-or)))

(defop eor [0x49 immediate        (cycles 2)
            0x45 zero-page        (cycles 3)
            0x55 zero-page-x      (cycles 4)
            0x4d absolute         (cycles 4)
            0x5d absolute-x       (cycles 4 :cross-page 1)
            0x59 absolute-y       (cycles 4 :cross-page 1)
            0x41 indexed-indirect (cycles 6)
            0x51 indirect-indexed (cycles 5 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (logical-op after-io address-mode operand bit-xor)))

(defop bit [0x24 zero-page (cycles 3)
            0x2c absolute  (cycles 4)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        result (unsigned-byte (bit-and (:a after-io) operand))
        overflowed? (= 0x40 (bit-and operand 0x40))]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag overflow-flag overflowed?)
      (set-flag negative-flag (negative? operand))
      (advance-pc address-mode))))

;; Load & store operations
(defn load-op
  [cpu mode operand reg]
  (let [result (unsigned-byte operand)]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result)
      (advance-pc mode))))

(defop lda [0xa9 immediate        (cycles 2)
            0xa5 zero-page        (cycles 3)
            0xb5 zero-page-x      (cycles 4)
            0xad absolute         (cycles 4)
            0xbd absolute-x       (cycles 4 :cross-page 1)
            0xb9 absolute-y       (cycles 4 :cross-page 1)
            0xa1 indexed-indirect (cycles 6)
            0xb1 indirect-indexed (cycles 5 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (load-op after-io address-mode operand :a)))

(defop ldx [0xa2 immediate   (cycles 2)
            0xa6 zero-page   (cycles 3)
            0xb6 zero-page-y (cycles 4)
            0xae absolute    (cycles 4)
            0xbe absolute-y  (cycles 4 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (load-op after-io address-mode operand :x)))

(defop ldy [0xa0 immediate   (cycles 2)
            0xa4 zero-page   (cycles 3)
            0xb4 zero-page-x (cycles 4)
            0xac absolute    (cycles 4)
            0xbc absolute-x  (cycles 4 :cross-page 1)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (load-op after-io address-mode operand :y)))

(defn store-op
  [cpu address-mode reg]
  (let [[_ after-store] (io-> cpu
                              (mode-write address-mode (reg cpu)))]
    (advance-pc after-store address-mode)))

(defop sta [0x85 zero-page        (cycles 3)
            0x95 zero-page-x      (cycles 4)
            0x8d absolute         (cycles 4)
            0x9d absolute-x       (cycles 5)
            0x99 absolute-y       (cycles 5)
            0x81 indexed-indirect (cycles 6)
            0x91 indirect-indexed (cycles 6)]
  (store-op cpu address-mode :a))

(defop stx [0x86 zero-page   (cycles 3)
            0x96 zero-page-y (cycles 4)
            0x8e absolute    (cycles 4)]
  (store-op cpu address-mode :x))

(defop sty [0x84 zero-page   (cycles 3)
            0x94 zero-page-x (cycles 4)
            0x8c absolute    (cycles 4)]
  (store-op cpu address-mode :y))

;; Register transfers
(defn transfer-reg
  [cpu from to]
  (assoc cpu to (from cpu)))

(defn transfer-reg-op
  [cpu from to]
  (let [result (from cpu)]
    (-> cpu
      (transfer-reg from to)
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop tax [0xaa implied (cycles 2)] (transfer-reg-op cpu :a :x))
(defop tay [0xa8 implied (cycles 2)] (transfer-reg-op cpu :a :y))
(defop txa [0x8a implied (cycles 2)] (transfer-reg-op cpu :x :a))
(defop tya [0x98 implied (cycles 2)] (transfer-reg-op cpu :y :a))
(defop tsx [0xba implied (cycles 2)] (transfer-reg-op cpu :sp :x))
(defop txs [0x9a implied (cycles 2)] (transfer-reg cpu :x :sp))

;; Increment & decrements
(defn increment-op
  [cpu reg]
  (let [result (unsigned-byte (inc (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop inc [0xe6 zero-page   (cycles 5)
            0xf6 zero-page-x (cycles 6)
            0xee absolute    (cycles 6)
            0xfe absolute-x  (cycles 7)]
   (let [[result after-io] ((with-io-> [orig (mode-read address-mode)
                                        incd (let [result (unsigned-byte (inc orig))]
                                               (mode-write address-mode result))]
                                       incd) cpu)]
     (-> after-io
       (set-flag zero-flag (zero? result))
       (set-flag negative-flag (negative? result))
       (advance-pc address-mode))))

(defop inx [0xe8 implied (cycles 2)] (increment-op cpu :x))
(defop iny [0xc8 implied (cycles 2)] (increment-op cpu :y))

(defn dec-reg-op
  [cpu reg]
  (let [result (unsigned-byte (dec (reg cpu)))]
    (-> cpu
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (assoc reg result))))

(defop dec [0xc6 zero-page   (cycles 5)
            0xd6 zero-page-x (cycles 6)
            0xce absolute    (cycles 6)
            0xde absolute-x  (cycles 7)]
  (let [[result after-io] ((with-io-> [before (mode-read address-mode)
                                       after (mode-write address-mode
                                               (unsigned-byte
                                                 (dec before)))]
                                      after) cpu)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result))
      (advance-pc address-mode))))

(defop dex [0xca implied (cycles 2)] (dec-reg-op cpu :x))
(defop dey [0x88 implied (cycles 2)] (dec-reg-op cpu :y))

;; Stack pushing and popping
(defn stack-push [cpu v]
  (let [pointer (+ 0x100 (:sp cpu))
        [_ after-push] (io-> cpu
                             (io-write v pointer))
        new-sp (unsigned-byte (dec (:sp cpu)))]
    (assoc after-push :sp new-sp)))

(defop pha [0x48 implied (cycles 3)] (stack-push cpu (:a cpu)))
(defop php [0x08 implied (cycles 3)] (stack-push cpu (bit-or 0x10 (:p cpu))))

(defn stack-pull [cpu]
  (let [top (unsigned-byte (inc (:sp cpu)))
        pointer (+ 0x100 top)
        [v after-pull] (io-> cpu (io-read pointer))
        new-sp (unsigned-byte (inc (:sp cpu)))]
    [v (assoc after-pull :sp new-sp)]))

(defn stack-pull-reg [cpu reg]
  (let [[v after-pull] (stack-pull cpu)]
    (assoc after-pull reg v)))

(defn stack-pull-pc [cpu]
  (let [[low after-low] (stack-pull cpu)
        [high after-pulls] (stack-pull after-low)
        new-pc (bit-or (bit-shift-left high 8) low)]
    (assoc after-pulls :pc new-pc)))

(defn stack-pull-flags [cpu]
  (let [pulled (stack-pull-reg cpu :p)]
    (-> pulled
      (set-flag break-flag false)
      (set-flag unused-flag true))))

(defn interrupt-vector [cpu]
  (io-> cpu (io-read-word 0xfffe)))

(defop pla [0x68 implied (cycles 4)]
  (let [pulled (stack-pull-reg cpu :a)
        result (:a pulled)]
    (-> pulled
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop plp [0x28 implied (cycles 4)] (stack-pull-flags cpu))

;; Jumps and calls
(defop jmp [0x4c absolute (cycles 3)
            0x6c indirect (cycles 5)]
  (let [[where after-io] (io-> cpu
                               (address-mode))]
    (assoc after-io :pc where)))

(defop jsr [0x20 absolute (cycles 6)]
  (let [return-pc (dec (:pc (advance-pc cpu address-mode)))
        high (high-byte return-pc)
        low  (low-byte return-pc)
        [where after-io] (io-> cpu
                               (address-mode))]
    (-> after-io
      (stack-push high)
      (stack-push low)
      (assoc :pc where))))

(defop rti [0x40 implied (cycles 6)]
  (-> cpu
    (stack-pull-flags)
    (stack-pull-pc)))

(defop rts [0x60 implied (cycles 6)]
  (let [pulled (stack-pull-pc cpu)]
    (assoc pulled :pc (inc (:pc pulled)))))

;; Branching
(defn branch-if [cpu mode predicate]
  (let [[addr after-io] (io-> cpu (relative))]
    (if predicate
      (with-meta (assoc after-io :pc addr) {:branched? true})
      (advance-pc after-io mode))))

(defop bcc [0x90 relative (branch-cycles 2)] (branch-if cpu address-mode (not (carry-flag? cpu))))
(defop bcs [0xb0 relative (branch-cycles 2)] (branch-if cpu address-mode (carry-flag? cpu)))
(defop beq [0xf0 relative (branch-cycles 2)] (branch-if cpu address-mode (zero-flag? cpu)))
(defop bmi [0x30 relative (branch-cycles 2)] (branch-if cpu address-mode (negative-flag? cpu)))
(defop bne [0xd0 relative (branch-cycles 2)] (branch-if cpu address-mode (not (zero-flag? cpu))))
(defop bpl [0x10 relative (branch-cycles 2)] (branch-if cpu address-mode (not (negative-flag? cpu))))
(defop bvc [0x50 relative (branch-cycles 2)] (branch-if cpu address-mode (not (overflow-flag? cpu))))
(defop bvs [0x70 relative (branch-cycles 2)] (branch-if cpu address-mode (overflow-flag? cpu)))

;; Status flag changes
(defop clc [0x18 implied (cycles 2)] (set-flag cpu carry-flag false))
(defop cld [0xd8 implied (cycles 2)] (set-flag cpu decimal-flag false))
(defop cli [0x58 implied (cycles 2)] (set-flag cpu interrupt-flag false))
(defop clv [0xb8 implied (cycles 2)] (set-flag cpu overflow-flag false))
(defop sec [0x38 implied (cycles 2)] (set-flag cpu carry-flag true))
(defop sed [0xf8 implied (cycles 2)] (set-flag cpu decimal-flag true))
(defop sei [0x78 implied (cycles 2)] (set-flag cpu interrupt-flag true))

;; System functions
(defop nop [0xea implied (cycles 2)] cpu)

(defop brk [0x00 implied (cycles 7)]
  (let [pc (+ 1 (:pc cpu))
        [interrupt after-read] (interrupt-vector cpu)
        high (high-byte pc)
        low  (low-byte pc)]
    (-> after-read
      (stack-push high)
      (stack-push low)
      (stack-push (bit-or 0x10 (:p after-read)))
      (set-flag interrupt-flag true)
      (assoc :pc interrupt))))

;; Shifts
(defn shift-mode-left [cpu address-mode]
  (let [[[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                       (unsigned-byte
                                                         (bit-shift-left before 1)))]
                                             [before after]) cpu)
        carried? (bit-set? orig 7)
        negative? (bit-set? result 7)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag negative?)
      (set-flag carry-flag carried?))))

(defop asl [0x0a accumulator (cycles 2)
            0x06 zero-page   (cycles 5)
            0x16 zero-page-x (cycles 6)
            0x0e absolute    (cycles 6)
            0x1e absolute-x  (cycles 7)]
    (-> cpu
      (shift-mode-left address-mode)
      (advance-pc address-mode)))

(defn shift-mode-right [cpu address-mode]
  (let [[[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                      (unsigned-byte
                                                        (bit-shift-right before 1)))]
                                             [before after]) cpu)
        carried? (bit-set? orig 0)]
    (-> after-io
      (set-flag carry-flag carried?)
      (set-flag negative-flag false)
      (set-flag zero-flag (zero? result)))))

(defop lsr [0x4a accumulator (cycles 2)
            0x46 zero-page   (cycles 5)
            0x56 zero-page-x (cycles 6)
            0x4e absolute    (cycles 6)
            0x5e absolute-x  (cycles 7)]
  (-> cpu
    (shift-mode-right address-mode)
    (advance-pc address-mode)))

(defn rotate-l [v carry?]
  (let [shifted (unsigned-byte (bit-shift-left v 1))]
    (if carry?
      (bit-or 1 shifted)
      shifted)))

(defn rotate-mode-left [cpu address-mode]
  (let [with-carry? (carry-flag? cpu)
        [[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                      (rotate-l before with-carry?))]
                                             [before after]) cpu)
        carried? (bit-set? orig 7)
        negative? (bit-set? result 7)]
    (-> after-io
      (set-flag negative-flag negative?)
      (set-flag zero-flag (zero? result))
      (set-flag carry-flag carried?))))

(defop rol [0x2a accumulator (cycles 2)
            0x26 zero-page   (cycles 5)
            0x36 zero-page-x (cycles 6)
            0x2e absolute    (cycles 6)
            0x3e absolute-x  (cycles 7)]
  (-> cpu
    (rotate-mode-left address-mode)
    (advance-pc address-mode)))

(defn rotate-r [v carry?]
  (let [shifted (unsigned-byte (bit-shift-right v 1))]
    (if carry?
      (bit-or 0x80 shifted)
      shifted)))

(defn rotate-mode-right [cpu address-mode]
  (let [with-carry? (carry-flag? cpu)
        [[orig result] after-io] ((with-io-> [before (mode-read address-mode)
                                              after (mode-write address-mode
                                                      (rotate-r before with-carry?))]
                                             [before after]) cpu)
        carried? (bit-set? orig 0)
        negative? (bit-set? result 7)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag negative?)
      (set-flag carry-flag carried?))))

(defop ror [0x6a accumulator (cycles 2)
            0x66 zero-page   (cycles 5)
            0x76 zero-page-x (cycles 6)
            0x6e absolute    (cycles 6)
            0x7e absolute-x  (cycles 7)]
  (-> cpu
    (rotate-mode-right address-mode)
    (advance-pc address-mode)))

;; Unofficial operations
(defop *nop [0x04 zero-page   (cycles 3)
             0x44 zero-page   (cycles 3)
             0x64 zero-page   (cycles 3)
             0x0c absolute    (cycles 4)
             0x14 zero-page-x (cycles 4)
             0x34 zero-page-x (cycles 4)
             0x54 zero-page-x (cycles 4)
             0x74 zero-page-x (cycles 4)
             0xd4 zero-page-x (cycles 4)
             0xf4 zero-page-x (cycles 4)
             0x1a implied     (cycles 2)
             0x3a implied     (cycles 2)
             0x5a implied     (cycles 2)
             0x7a implied     (cycles 2)
             0xda implied     (cycles 2)
             0xfa implied     (cycles 2)
             0x80 immediate   (cycles 2)
             0x82 immediate   (cycles 2)
             0x89 immediate   (cycles 2)
             0xc2 immediate   (cycles 2)
             0xe2 immediate   (cycles 2)
             0x1c absolute-x  (cycles 4 :cross-page 1)
             0x3c absolute-x  (cycles 4 :cross-page 1)
             0x5c absolute-x  (cycles 4 :cross-page 1)
             0x7c absolute-x  (cycles 4 :cross-page 1)
             0xdc absolute-x  (cycles 4 :cross-page 1)
             0xfc absolute-x  (cycles 4 :cross-page 1)]
       (advance-pc cpu address-mode))

(defop *lax [0xab immediate        (cycles 2)
             0xa3 indexed-indirect (cycles 6)
             0xa7 zero-page        (cycles 3)
             0xaf absolute         (cycles 4)
             0xb3 indirect-indexed (cycles 5 :cross-page 1)
             0xb7 zero-page-y      (cycles 4)
             0xbf absolute-y       (cycles 4 :cross-page 1)]
  (let [[v after-io] (io-> cpu (mode-read address-mode))]
    (-> after-io
      (set-flag zero-flag (zero? v))
      (set-flag negative-flag (negative? v))
      (assoc :x v)
      (assoc :a v)
      (advance-pc address-mode))))

(defop *sax [0x83 indexed-indirect (cycles 6)
             0x87 zero-page        (cycles 3)
             0x8f absolute         (cycles 4)
             0x97 zero-page-y      (cycles 4)]
  (let [v (bit-and (:a cpu) (:x cpu))
        [_ after-io] (io-> cpu
                           (mode-write address-mode v))]
    (advance-pc after-io address-mode)))

(defop *sbc [0xeb immediate (cycles 2)]
  ((op :sbc) cpu address-mode))

(defop *dcp [0xc3 indexed-indirect (cycles 8)
             0xd3 indirect-indexed (cycles 8)
             0xc7 zero-page        (cycles 5)
             0xcf absolute         (cycles 6)
             0xd7 zero-page-x      (cycles 6)
             0xdb absolute-y       (cycles 7)
             0xdf absolute-x       (cycles 7)]
  (let [[operand after-io] ((with-io-> [orig (mode-read address-mode)
                                        decd (mode-write
                                               address-mode
                                               (unsigned-byte (dec orig)))]
                                       decd) cpu)]
    (-> after-io
      (compare-op :a operand)
      (advance-pc address-mode))))

(defop *isb [0xe3 indexed-indirect (cycles 8)
             0xe7 zero-page        (cycles 5)
             0xef absolute         (cycles 6)
             0xf3 indirect-indexed (cycles 8)
             0xf7 zero-page-x      (cycles 6)
             0xfb absolute-y       (cycles 7)
             0xff absolute-x       (cycles 7)]
  (let [[incd after-io] ((with-io-> [orig (mode-read address-mode)
                                     incd (mode-write
                                            address-mode
                                            (unsigned-byte (inc orig)))]
                                    incd) cpu)]
    (-> after-io
      (subtract-op incd)
      (advance-pc address-mode))))

(defop *slo [0x03 indexed-indirect (cycles 8)
             0x07 zero-page        (cycles 5)
             0x0f absolute         (cycles 6)
             0x13 indirect-indexed (cycles 8)
             0x17 zero-page-x      (cycles 6)
             0x1b absolute-y       (cycles 7)
             0x1f absolute-x       (cycles 7)]
  (let [after-shift (shift-mode-left cpu address-mode)
        [shifted after-read] (io-> after-shift (mode-read address-mode))]
    (logical-op after-read address-mode shifted bit-or)))

(defop *rla [0x23 indexed-indirect (cycles 8)
             0x27 zero-page        (cycles 5)
             0x2f absolute         (cycles 6)
             0x33 indirect-indexed (cycles 8)
             0x37 zero-page-x      (cycles 6)
             0x3b absolute-y       (cycles 7)
             0x3f absolute-x       (cycles 7)]
  (let [after-rotate (rotate-mode-left cpu address-mode)
        [rotated after-read] (io-> after-rotate (mode-read address-mode))]
    (logical-op after-read address-mode rotated bit-and)))

(defop *sre [0x43 indexed-indirect (cycles 8)
             0x47 zero-page        (cycles 5)
             0x4f absolute         (cycles 6)
             0x53 indirect-indexed (cycles 8)
             0x57 zero-page-x      (cycles 6)
             0x5b absolute-y       (cycles 7)
             0x5f absolute-x       (cycles 7)]
  (let [after-shift (shift-mode-right cpu address-mode)
        [shifted after-read] (io-> after-shift (mode-read address-mode))]
    (logical-op after-read address-mode shifted bit-xor)))

(defop *rra [0x63 indexed-indirect (cycles 8)
             0x67 zero-page        (cycles 5)
             0x6f absolute         (cycles 6)
             0x73 indirect-indexed (cycles 8)
             0x77 zero-page-x      (cycles 6)
             0x7b absolute-y       (cycles 7)
             0x7f absolute-x       (cycles 7)]
  (let [after-rotate (rotate-mode-right cpu address-mode)
        [rotated after-read] (io-> after-rotate (mode-read address-mode))]
    (-> after-read
      (add-op rotated)
      (advance-pc address-mode))))

(defop *anc [0x0b immediate (cycles 2)
             0x2b immediate (cycles 2)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        after-and (logical-op after-io address-mode operand bit-and)
        carried? (bit-set? (:a after-and) 7)]
    (-> after-and
      (set-flag carry-flag carried?))))

(defop *alr [0x4b immediate (cycles 2)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        after-and (logical-op after-io address-mode operand bit-and)]
    (shift-mode-right after-and accumulator)))

(defop *axs [0xcb immediate (cycles 2)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        anded (bit-and (:a after-io) (:x after-io))
        subbed (- anded operand)
        result (unsigned-byte subbed)
        carried? (>= anded operand)]
    (-> after-io
      (assoc :x result)
      (set-flag carry-flag carried?)
      (set-flag negative-flag (negative? result))
      (set-flag zero-flag (zero? result))
      (advance-pc address-mode))))

(defop *arr [0x6b immediate (cycles 2)]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        anded (bit-and (:a after-io) operand)
        rotated (rotate-r anded (carry-flag? after-io))
        carried? (bit-set? rotated 6)
        overflowed? (not= 0
                          (bit-xor
                            (bit-and rotated 0x40)
                            (bit-shift-left (bit-and rotated 0x20) 1)))]
    (-> after-io
      (assoc :a rotated)
      (set-flag negative-flag (negative? rotated))
      (set-flag zero-flag (zero? rotated))
      (set-flag carry-flag carried?)
      (set-flag overflow-flag overflowed?)
      (advance-pc address-mode))))

(defn- sh*-op [cpu address-mode reg]
  (let [[addr after-io] (io-> cpu (address-mode))
        high (unsigned-byte (+ 1 (high-byte addr)))
        result (bit-and (reg after-io) high)
        [_ after-write] (io-> after-io
                              (mode-write address-mode result))]
    (-> after-write
      (advance-pc address-mode))))

(defop *shy [0x9c absolute-x (cycles 5)] (sh*-op cpu address-mode :y))
(defop *shx [0x9e absolute-y (cycles 5)] (sh*-op cpu address-mode :x))
