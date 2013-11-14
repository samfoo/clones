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
    `(let [~'op-fn (fn ~fn-args ~action)]
       (def ops (assoc ops (keyword '~op-name) ~'op-fn))
       (def op-codes
         (reduce (fn [~'m ~'op]
                   (assoc ~'m
                          (first ~'op)
                          (with-meta ~'op-fn {:address-mode (second ~'op)
                                              :name (name '~op-name)})))
                 op-codes
                 (partition 2 ~opcodes))))))

(defn make-cpu []
  (CPU. 0 0 0 0xfd 0x24 0 (-> {}
                            (mount-device :internal-ram 0 0x1fff {}))))

(defn- inc-pc [cpu]
  (assoc cpu :pc (inc (:pc cpu))))

(defn- execute [cpu op]
  (let [{:keys [address-mode name]} (meta op)]
    (op cpu address-mode)))

(defn- advance-pc [cpu mode]
  (assoc cpu :pc (+
                   (:pc cpu)
                   (mode-size mode))))

(defn step [cpu]
  (let [[op-code after-read] (io-> cpu (io-read (:pc cpu)))
        op (get op-codes op-code)]
    (if (nil? op)
      (throw (ex-info (format "Invalid op code $%02x" op-code) {:op-code op-code}))
      nil)
    (execute (inc-pc after-read) op)))

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

(defop cmp [0xc9 immediate
            0xc5 zero-page
            0xd5 zero-page-x
            0xcd absolute
            0xdd absolute-x
            0xd9 absolute-y
            0xc1 indexed-indirect
            0xd1 indirect-indexed]
  (compare-op-with-io cpu address-mode :a))

(defop cpx [0xe0 immediate
            0xe4 zero-page
            0xec absolute]
  (compare-op-with-io cpu address-mode :x))

(defop cpy [0xc0 immediate
            0xc4 zero-page
            0xcc absolute]
  (compare-op-with-io cpu address-mode :y))

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

(defop adc [0x69 immediate
            0x65 zero-page
            0x75 zero-page-x
            0x6d absolute
            0x7d absolute-x
            0x79 absolute-y
            0x61 indexed-indirect
            0x71 indirect-indexed]
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

(defop sbc [0xe9 immediate
            0xe5 zero-page
            0xf5 zero-page-x
            0xed absolute
            0xfd absolute-x
            0xf9 absolute-y
            0xe1 indexed-indirect
            0xf1 indirect-indexed]
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

(defop and [0x29 immediate
            0x25 zero-page
            0x35 zero-page-x
            0x2d absolute
            0x3d absolute-x
            0x39 absolute-y
            0x21 indexed-indirect
            0x31 indirect-indexed]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (logical-op after-io address-mode operand bit-and)))

(defop ora [0x09 immediate
            0x05 zero-page
            0x15 zero-page-x
            0x0d absolute
            0x1d absolute-x
            0x19 absolute-y
            0x01 indexed-indirect
            0x11 indirect-indexed]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (logical-op after-io address-mode operand bit-or)))

(defop eor [0x49 immediate
            0x45 zero-page
            0x55 zero-page-x
            0x4d absolute
            0x5d absolute-x
            0x59 absolute-y
            0x41 indexed-indirect
            0x51 indirect-indexed]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (logical-op after-io address-mode operand bit-xor)))

(defop bit [0x24 zero-page
            0x2c absolute]
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

(defop lda [0xa9 immediate
            0xa5 zero-page
            0xb5 zero-page-x
            0xad absolute
            0xbd absolute-x
            0xb9 absolute-y
            0xa1 indexed-indirect
            0xb1 indirect-indexed]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (load-op after-io address-mode operand :a)))

(defop ldx [0xa2 immediate
            0xa6 zero-page
            0xb6 zero-page-y
            0xae absolute
            0xbe absolute-y]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (load-op after-io address-mode operand :x)))

(defop ldy [0xa0 immediate
            0xa4 zero-page
            0xb4 zero-page-x
            0xac absolute
            0xbc absolute-x]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))]
    (load-op after-io address-mode operand :y)))

(defn store-op
  [cpu address-mode reg]
  (let [[_ after-store] (io-> cpu
                              (mode-write address-mode (reg cpu)))]
    (advance-pc after-store address-mode)))

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

(defop tax [0xaa implied] (transfer-reg-op cpu :a :x))
(defop tay [0xa8 implied] (transfer-reg-op cpu :a :y))
(defop txa [0x8a implied] (transfer-reg-op cpu :x :a))
(defop tya [0x98 implied] (transfer-reg-op cpu :y :a))
(defop tsx [0xba implied] (transfer-reg-op cpu :sp :x))
(defop txs [0x9a implied] (transfer-reg cpu :x :sp))

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
       (set-flag negative-flag (negative? result))
       (advance-pc address-mode))))

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
      (set-flag negative-flag (negative? result))
      (advance-pc address-mode))))

(defop dex [0xca implied] (dec-reg-op cpu :x))
(defop dey [0x88 implied] (dec-reg-op cpu :y))

;; Stack pushing and popping
(defn stack-push [cpu v]
  (let [pointer (+ 0x100 (:sp cpu))
        [_ after-push] (io-> cpu
                             (io-write v pointer))
        new-sp (unsigned-byte (dec (:sp cpu)))]
    (assoc after-push :sp new-sp)))

(defop pha [0x48 implied] (stack-push cpu (:a cpu)))
(defop php [0x08 implied] (stack-push cpu (bit-or 0x10 (:p cpu))))

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

(defop pla [0x68 implied]
  (let [pulled (stack-pull-reg cpu :a)
        result (:a pulled)]
    (-> pulled
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag (negative? result)))))

(defop plp [0x28 implied] (stack-pull-flags cpu))

;; Jumps and calls
(defop jmp [0x4c absolute
            0x6c indirect]
  (let [[where after-io] (io-> cpu
                               (address-mode))]
    (assoc after-io :pc where)))

(defop jsr [0x20 absolute]
  (let [return-pc (dec (:pc (advance-pc cpu address-mode)))
        high (high-byte return-pc)
        low  (low-byte return-pc)
        [where after-io] (io-> cpu
                               (address-mode))]
    (-> after-io
      (stack-push high)
      (stack-push low)
      (assoc :pc where))))

(defop rti [0x40 implied]
  (-> cpu
    (stack-pull-flags)
    (stack-pull-pc)))

(defop rts [0x60 implied]
  (let [pulled (stack-pull-pc cpu)]
    (assoc pulled :pc (inc (:pc pulled)))))

;; Branching
(defn branch-if [cpu mode predicate]
  (let [[addr after-io] (io-> cpu (relative))]
    (if predicate
      (assoc after-io :pc addr)
      (advance-pc after-io mode))))

(defop bcc [0x90 relative] (branch-if cpu address-mode (not (carry-flag? cpu))))
(defop bcs [0xb0 relative] (branch-if cpu address-mode (carry-flag? cpu)))
(defop beq [0xf0 relative] (branch-if cpu address-mode (zero-flag? cpu)))
(defop bmi [0x30 relative] (branch-if cpu address-mode (negative-flag? cpu)))
(defop bne [0xd0 relative] (branch-if cpu address-mode (not (zero-flag? cpu))))
(defop bpl [0x10 relative] (branch-if cpu address-mode (not (negative-flag? cpu))))
(defop bvc [0x50 relative] (branch-if cpu address-mode (not (overflow-flag? cpu))))
(defop bvs [0x70 relative] (branch-if cpu address-mode (overflow-flag? cpu)))

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
        carried? (bit-set? orig 0x80)
        negative? (bit-set? result 0x80)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag negative?)
      (set-flag carry-flag carried?))))

(defop asl [0x0a accumulator
            0x06 zero-page
            0x16 zero-page-x
            0x0e absolute
            0x1e absolute-x]
    (-> cpu
      (shift-mode-left address-mode)
      (advance-pc address-mode)))

(defn shift-mode-right [cpu address-mode]
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

(defop lsr [0x4a accumulator
            0x46 zero-page
            0x56 zero-page-x
            0x4e absolute
            0x5e absolute-x]
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
        carried? (bit-set? orig 0x80)
        negative? (bit-set? result 0x80)]
    (-> after-io
      (set-flag negative-flag negative?)
      (set-flag zero-flag (zero? result))
      (set-flag carry-flag carried?))))

(defop rol [0x2a accumulator
            0x26 zero-page
            0x36 zero-page-x
            0x2e absolute
            0x3e absolute-x]
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
        carried? (bit-set? orig 1)
        negative? (bit-set? result 0x80)]
    (-> after-io
      (set-flag zero-flag (zero? result))
      (set-flag negative-flag negative?)
      (set-flag carry-flag carried?))))

(defop ror [0x6a accumulator
            0x66 zero-page
            0x76 zero-page-x
            0x6e absolute
            0x7e absolute-x]
  (-> cpu
    (rotate-mode-right address-mode)
    (advance-pc address-mode)))

;; Unofficial operations
(defop *nop [0x04 zero-page
             0x44 zero-page
             0x64 zero-page
             0x0c absolute
             0x14 zero-page-x
             0x34 zero-page-x
             0x54 zero-page-x
             0x74 zero-page-x
             0xd4 zero-page-x
             0xf4 zero-page-x
             0x1a implied
             0x3a implied
             0x5a implied
             0x7a implied
             0xda implied
             0xfa implied
             0x80 immediate
             0x82 immediate
             0x89 immediate
             0xc2 immediate
             0xe2 immediate
             0x1c absolute-x
             0x3c absolute-x
             0x5c absolute-x
             0x7c absolute-x
             0xdc absolute-x
             0xfc absolute-x]
       (advance-pc cpu address-mode))

(defop *lax [0xab immediate
             0xa3 indexed-indirect
             0xa7 zero-page
             0xaf absolute
             0xb3 indirect-indexed
             0xb7 zero-page-y
             0xbf absolute-y]
  (let [[v after-io] (io-> cpu (mode-read address-mode))]
    (-> after-io
      (set-flag zero-flag (zero? v))
      (set-flag negative-flag (negative? v))
      (assoc :x v)
      (assoc :a v)
      (advance-pc address-mode))))

(defop *sax [0x83 indexed-indirect
             0x87 zero-page
             0x8f absolute
             0x97 zero-page-y]
  (let [v (bit-and (:a cpu) (:x cpu))
        [_ after-io] (io-> cpu
                           (mode-write address-mode v))]
    (advance-pc after-io address-mode)))

(defop *sbc [0xeb immediate]
  ((op :sbc) cpu address-mode))

(defop *dcp [0xc3 indexed-indirect
             0xd3 indirect-indexed
             0xc7 zero-page
             0xcf absolute
             0xd7 zero-page-x
             0xdb absolute-y
             0xdf absolute-x]
  (let [[operand after-io] ((with-io-> [orig (mode-read address-mode)
                                        decd (mode-write
                                               address-mode
                                               (unsigned-byte (dec orig)))]
                                       decd) cpu)]
    (-> after-io
      (compare-op :a operand)
      (advance-pc address-mode))))

(defop *isb [0xe3 indexed-indirect
             0xe7 zero-page
             0xef absolute
             0xf3 indirect-indexed
             0xf7 zero-page-x
             0xfb absolute-y
             0xff absolute-x]
  (let [[incd after-io] ((with-io-> [orig (mode-read address-mode)
                                     incd (mode-write
                                            address-mode
                                            (unsigned-byte (inc orig)))]
                                    incd) cpu)]
    (-> after-io
      (subtract-op incd)
      (advance-pc address-mode))))

(defop *slo [0x03 indexed-indirect
             0x07 zero-page
             0x0f absolute
             0x13 indirect-indexed
             0x17 zero-page-x
             0x1b absolute-y
             0x1f absolute-x]
  (let [after-shift (shift-mode-left cpu address-mode)
        [shifted after-read] (io-> after-shift (mode-read address-mode))]
    (logical-op after-read address-mode shifted bit-or)))

(defop *rla [0x23 indexed-indirect
             0x27 zero-page
             0x2f absolute
             0x33 indirect-indexed
             0x37 zero-page-x
             0x3b absolute-y
             0x3f absolute-x]
  (let [after-rotate (rotate-mode-left cpu address-mode)
        [rotated after-read] (io-> after-rotate (mode-read address-mode))]
    (logical-op after-read address-mode rotated bit-and)))

(defop *sre [0x43 indexed-indirect
             0x47 zero-page
             0x4f absolute
             0x53 indirect-indexed
             0x57 zero-page-x
             0x5b absolute-y
             0x5f absolute-x]
  (let [after-shift (shift-mode-right cpu address-mode)
        [shifted after-read] (io-> after-shift (mode-read address-mode))]
    (logical-op after-read address-mode shifted bit-xor)))

(defop *rra [0x63 indexed-indirect
             0x67 zero-page
             0x6f absolute
             0x73 indirect-indexed
             0x77 zero-page-x
             0x7b absolute-y
             0x7f absolute-x]
  (let [after-rotate (rotate-mode-right cpu address-mode)
        [rotated after-read] (io-> after-rotate (mode-read address-mode))]
    (-> after-read
      (add-op rotated)
      (advance-pc address-mode))))

(defop *anc [0x0b immediate
             0x2b immediate]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        after-and (logical-op after-io address-mode operand bit-and)
        carried? (bit-set? (:a after-and) 0x80)]
    (-> after-and
      (set-flag carry-flag carried?))))

(defop *alr [0x4b immediate]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        after-and (logical-op after-io address-mode operand bit-and)]
    (shift-mode-right after-and accumulator)))

(defop *axs [0xcb immediate]
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

(defop *arr [0x6b immediate]
  (let [[operand after-io] (io-> cpu (mode-read address-mode))
        anded (bit-and (:a after-io) operand)
        rotated (rotate-r anded (carry-flag? after-io))
        carried? (bit-set? rotated 0x40)
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

(defop *shy [0x9c absolute-x] (sh*-op cpu address-mode :y))
(defop *shx [0x9e absolute-y] (sh*-op cpu address-mode :x))
