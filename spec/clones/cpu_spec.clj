(ns clones.cpu-spec
  (:require [speclj.core       :refer :all]
            [clones.cpu        :refer :all]
            [clones.memory     :refer :all]
            [clones.addressing :refer :all]))

(def cpu (make-cpu))
(def cpu-with-carry (assoc cpu :p carry-flag))
(def cpu-with-zero (assoc cpu :p zero-flag))
(def cpu-with-negative (assoc cpu :p negative-flag))
(def cpu-with-decimal (assoc cpu :p decimal-flag))
(def cpu-with-interrupt (assoc cpu :p interrupt-flag))
(def cpu-with-overflow (assoc cpu :p overflow-flag))

(describe "A 6502 CPU"
  (describe "instruction set"
    (defn check-zero-flag-sets [c]
      (it "should set the zero flag when the result is zero"
        (should (zero-flag? (c cpu)))))

    (defn check-zero-flag-unsets [c]
      (it "should unset the zero flag when the result is non-zero"
        (should-not (zero-flag? (c (set-flag cpu zero-flag true))))))

    (defn check-negative-flag-sets [c]
      (it "should set the negative flag when the result is negative"
        (should (negative-flag? (c cpu)))))

    (defn check-negative-flag-unsets [c]
      (it "should unset the negative flag when the result is non-negative"
        (should-not (negative-flag? (c (set-flag cpu negative-flag true))))))

    (defn check-carry-flag-unsets [c]
      (it "should unset the carry flag when the result doesn't carry"
        (should-not (carry-flag? (c cpu)))))

    (defn check-carry-flag-sets [c]
      (it "should set the carry flag when the result carries"
        (should (carry-flag? (c cpu)))))

    (defn peek-stack-n [c n]
      (mount-read (:memory c) (+ 0x100 1 n (:sp c))))

    (defn peek-stack [c] (peek-stack-n c 0))

    (defn with-stack-top [cpu v]
      (let [stack-with-value (mount-write (:memory cpu) v 0x1fd)]
        (merge cpu {:memory stack-with-value :sp 0xfc})))

    (defn check-branching [desc f should-branch should-not-branch]
      (it desc
        (should= 0 (:pc (f should-not-branch :operand 0xffff)))
        (should= 0xffff (:pc (f should-branch :operand 0xffff)))))

    (describe "shifts and rotates"
      (describe "asl"
        (check-zero-flag-sets #(*asl %1 :address-mode accumulator))
        (check-zero-flag-unsets #(*asl (assoc %1 :a 1) :address-mode accumulator))

        (check-negative-flag-sets #(*asl (assoc %1 :a 0x40) :address-mode accumulator))
        (check-negative-flag-unsets #(*asl (assoc %1 :a 1) :address-mode accumulator))

        (check-carry-flag-sets #(*asl (assoc %1 :a 0x80) :address-mode accumulator))
        (check-carry-flag-unsets #(*asl (assoc %1 :a 1) :address-mode accumulator))

        (it "should shift bits of the address mode left by 1 bit"
          (let [cpu-shifted (-> cpu
                              (assoc :a 1)
                              (*asl :address-mode accumulator))]
            (should= 2 (:a cpu-shifted))))))

    (describe "system functions"
      (describe "brk"
        (it "should set the program counter to the value at 0xfffe (the IRQ/BRK vector)"
          (let [cpu-with-vector (-> cpu
                                  (mem-write 0xff 0xffff)
                                  (mem-write 0xee 0xfffe))
                new-cpu (*brk cpu-with-vector)]
            (should= 0xffee (:pc new-cpu))))

        (it "should push the current program counter (plus 1) to the stack"
          (let [new-cpu (*brk (assoc cpu :pc 0xbeef))
                low (peek-stack-n new-cpu 1)
                high (peek-stack-n new-cpu 2)]
            (should= 0xbe high)
            (should= 0xf0 low)))

        (it "should push processor flags to the stack with the interrupt flag set"
          (let [new-cpu (*brk cpu)
                stack-top (peek-stack new-cpu)]
            (should= 0x10 stack-top))))

      (describe "nop"
        (it "should do nothing"
          (should= cpu (*nop cpu)))))

    (describe "status flag changes"
      (describe "sei"
        (it "should set the interrupt flag"
          (should= true (interrupt-flag? (*sei cpu)))))

      (describe "sed"
        (it "should set the decimal flag"
          (should= true (decimal-flag? (*sed cpu)))))

      (describe "sec"
        (it "should set the carry flag"
          (should= true (carry-flag? (*sec cpu)))))

      (describe "clv"
        (it "should clear the overflow flag"
          (should= false (overflow-flag? (*clv cpu-with-overflow)))))

      (describe "cli"
        (it "should clear the interrupt flag"
          (should= false (interrupt-flag? (*cli cpu-with-interrupt)))))

      (describe "cld"
        (it "should clear the decimal flag"
          (should= false (decimal-flag? (*cld cpu-with-decimal)))))

      (describe "clc"
        (it "should clear the carry flag"
          (should= false (carry-flag? (*clc cpu-with-carry))))))

    (describe "branching"
      (describe "bvs"
        (check-branching "should branch when overflow set"
                         *bvs
                         cpu-with-overflow
                         cpu))
      (describe "bvc"
        (check-branching "should branch when overflow clear"
                         *bvc
                         cpu
                         cpu-with-overflow))
      (describe "bpl"
        (check-branching "should branch when negative clear"
                         *bpl
                         cpu
                         cpu-with-negative))
      (describe "bne"
        (check-branching "should branch when zero clear"
                         *bne
                         cpu
                         cpu-with-zero))
      (describe "bmi"
        (check-branching "should branch when negative set"
                         *bmi
                         cpu-with-negative
                         cpu))
      (describe "beq"
        (check-branching "should branch when zero set"
                         *beq
                         cpu-with-zero
                         cpu))
      (describe "bcs"
        (check-branching "should branch when carry set"
                         *bcs
                         cpu-with-carry
                         cpu))
      (describe "bcc"
        (check-branching "should branch when carry clear"
                         *bcc
                         cpu
                         cpu-with-carry)))

    (describe "jumps and calls"
      (describe "rts"
        (it "should pull the program counter from the stack and then add one to it"
          (let [cpu-with-pc (-> cpu
                              (push 0xff)
                              (push 0xdd))
                new-cpu (*rts cpu-with-pc)]
            (should= 0xffde (:pc new-cpu)))))

      (describe "rti"
        (it "should pull the program counter from the stack after pulling flags"
          (let [cpu-with-p-and-pc (-> cpu
                                    (push 0xff)
                                    (push 0xdd)
                                    (push 0))
                new-cpu (*rti cpu-with-p-and-pc)]
            (should= 0xffdd (:pc new-cpu))))

        (it "should pull the flags from the top of the stack (break flag is always 0)"
          (let [new-cpu (*rti (with-stack-top cpu 0xff))]
            (should= 0xef (:p new-cpu)))))

      (describe "jsr"
        (it "should set the program counter to the argument"
          (let [new-cpu (*jsr cpu :operand 0xbeef)]
            (should= 0xbeef (:pc new-cpu))))

        (it "should push the current program counter (minus 1) to the stack"
          (let [new-cpu (*jsr (assoc cpu :pc 0xffdd) :operand 0)]
            (should= 0xdc (peek-stack-n new-cpu 0))
            (should= 0xff (peek-stack-n new-cpu 1)))))

      (describe "jmp"
        (it "should set the program counter to the argument"
          (let [new-cpu (*jmp cpu :operand 0x1000)]
            (should= 0x1000 (:pc new-cpu))))))

    (describe "stack operations"
      (describe "plp"
        (it "should pull the 5th bit as 1, no matter what"
          (let [new-cpu (*plp (with-stack-top cpu 0x00))]
            (should= 0x20 (:p new-cpu))))

        (it "should pull the break flag as 0, no matter what"
          (let [new-cpu (*plp (with-stack-top cpu 0xff))]
            (should= 0xef (:p new-cpu))))

        (it "should pull the top of the stack into the flags register"
          (let [new-cpu (*plp (with-stack-top cpu 0x21))]
            (should= 0x21 (:p new-cpu)))))

      (describe "pla"
        (check-zero-flag-sets #(*pla (with-stack-top % 0x00)))
        (check-zero-flag-unsets #(*pla (with-stack-top % 0x01)))
        (check-negative-flag-sets #(*pla (with-stack-top % 0x80)))
        (check-negative-flag-unsets #(*pla (with-stack-top % 0x00)))

        (it "should pull the top of the stack into the accumulator"
          (let [new-cpu (*pla (with-stack-top cpu 0xff))]
            (should= 0xff (:a new-cpu)))))

      (describe "php"
        (it "should always push the break flag as 1"
          ;; NOTE: This is only in the NES's 6502... should there be a way of
          ;; setting it so that the CPU can behave either way?
          (let [new-cpu (*php cpu)
                stack-top (peek-stack new-cpu)]
            (should= 0 (:p cpu))
            (should= break-flag stack-top)))

        (it "should push processor flags to the stack"
          (let [new-cpu (*php cpu-with-carry)
                stack-top (peek-stack new-cpu)]
            (should= (bit-or carry-flag break-flag) stack-top))))

      (describe "pha"
        (it "should wrap the stack pointer to 0xff if it is 0x00"
          (let [cpu-with-0-sp (assoc cpu :sp 0x00)
                new-cpu (*pha cpu-with-0-sp)]
            (should= 0xff (:sp new-cpu))))

        (it "should decrement the stack pointer"
          (let [new-cpu (*pha (assoc cpu :a 0xbe))
                new-sp (:sp new-cpu)]
            (should= (dec (:sp cpu)) (:sp new-cpu))))

        (it "should push the accumulator to the stack"
          (let [new-cpu (*pha (assoc cpu :a 0xbe))
                stack-top (peek-stack new-cpu)]
            (should= 0xbe stack-top)))))

    (describe "decrement operations"
      (map (fn [[op reg]]
             (describe (str op)
               (check-zero-flag-sets #(op (assoc %1 reg 1)))
               (check-zero-flag-unsets #(op (assoc %1 reg 2)))
               (check-negative-flag-sets #(op (assoc %1 reg 0)))
               (check-negative-flag-unsets #(op (assoc %1 reg 1)))

               (it (format "should decrement the %s register by one" (str reg))
                 (let [new-cpu (op cpu)]
                   (should= 0xff (reg new-cpu))))))
        {*dec :a
         *dex :x
         *dey :y}))

    (describe "increment operations"
      (map (fn [[op reg]]
             (describe (str op)
               (check-zero-flag-sets #(op (assoc %1 reg 0xff)))
               (check-zero-flag-unsets #(op (assoc %1 reg 0)))
               (check-negative-flag-sets #(op (assoc %1 reg 0x7f)))
               (check-negative-flag-unsets #(op (assoc %1 reg 0)))

               (it (format "should increment the %s register by one" (str reg))
                 (let [new-cpu (op cpu)]
                   (should= 1 (reg new-cpu))))))
        {*inc :a
         *inx :x
         *iny :y}))

    (describe "register transfer operations"
      (map (fn [[op [from-reg to-reg]]]
             (describe (str op)
               (check-zero-flag-sets #(op (assoc %1 from-reg 0)))
               (check-zero-flag-unsets #(op (assoc %1 from-reg 1)))
               (check-negative-flag-sets #(op (assoc %1 from-reg 0x80)))
               (check-negative-flag-unsets #(op (assoc %1 from-reg 0)))

               (it (format "should transfer the value in the %s register to the %s register" (str from-reg) (str to-reg))
                 (let [new-cpu (op (assoc cpu from-reg 0x44))]
                   (should= (to-reg new-cpu) 0x44)))))
        {*tax [:a :x]
         *tay [:a :y]
         *txa [:x :a]
         *tya [:y :a]
         *tsx [:sp :x]
         *txs [:x :sp]}))

    (describe "loading operations"
      (for [op [*lda *ldx *ldy]]
        (describe (str op)
          (check-zero-flag-sets #(op %1 :operand 0))
          (check-zero-flag-unsets #(op %1 :operand 1))
          (check-negative-flag-sets #(op %1 :operand 0x80))
          (check-negative-flag-unsets #(op %1 :operand 0))))

      (describe "ldy"
        (it "should load the y register with the argument"
          (let [new-cpu (*ldy cpu :operand 0xbb)]
            (should= (:y new-cpu) 0xbb))))

      (describe "ldx"
        (it "should load the x register with the argument"
          (let [new-cpu (*ldx cpu :operand 0xbb)]
            (should= (:x new-cpu) 0xbb))))

      (describe "lda"
        (it "should load the accumulator with the argument"
          (let [new-cpu (*lda cpu :operand 0xbb)]
            (should= (:a new-cpu) 0xbb)))))

    (describe "comparison operations"
      (map (fn [[op reg]]
        (describe (str op)
          (check-zero-flag-sets #(op %1 :operand 0))
          (check-zero-flag-unsets #(op (assoc %1 reg 1) :operand 0))
          (check-negative-flag-sets #(op (assoc %1 reg 0x81) :operand 1))
          (check-negative-flag-unsets #(op %1 :operand 0))

          (it (format "should set the carry flag if the %s register is greater than or equal to the argument" (str reg))
            (let [new-cpu (op (assoc cpu reg 0x10) :operand 1)]
              (should (carry-flag? new-cpu))))

          (it (format "should unset the carry flag if the %s register is less than the argument" (str reg))
            (let [cpu-with-carry (set-flag cpu overflow-flag true)
                  new-cpu (op cpu-with-carry :operand 0x10)]
              (should-not (carry-flag? new-cpu))))))
        {*cmp :a
         *cpx :x
         *cpy :y}))

    (describe "logical operations"
      (describe "bit"
        (check-zero-flag-sets #(*bit %1 :operand 0))
        (check-zero-flag-unsets #(*bit (assoc %1 :a 1) :operand 1))
        (check-negative-flag-sets #(*bit (assoc %1 :a 0x80) :operand 0x80))
        (check-negative-flag-unsets #(*bit %1 :operand 0))

        (it "should set the overflow flag when the 6th bit of the result is set"
          (let [new-cpu (*bit (assoc cpu :a 0x40) :operand 0x40)]
            (should (overflow-flag? new-cpu))))

        (it "should unset the overflow flag when the 6th bit of the result is unset"
          (let [new-cpu (*bit (set-flag cpu overflow-flag true) :operand 0)]
            (should-not (overflow-flag? new-cpu)))))

      (describe "eor"
        (check-zero-flag-sets #(*eor %1 :operand 0))
        (check-zero-flag-unsets #(*eor (assoc %1 :a 1) :operand 0))
        (check-negative-flag-sets #(*eor (assoc %1 :a 0x80) :operand 0))
        (check-negative-flag-unsets #(*eor %1 :operand 0)))

      (describe "ora"
        (check-zero-flag-sets #(*ora %1 :operand 0))
        (check-zero-flag-unsets #(*ora (assoc %1 :a 1) :operand 1))
        (check-negative-flag-sets #(*ora (assoc %1 :a 0x80) :operand 0x80))
        (check-negative-flag-unsets #(*ora %1 :operand 0))

        (it "should or the argument with the accumulator"
          (let [new-cpu (*ora cpu :operand 0xa5)]
            (should= (:a new-cpu) 0xa5))))

      (describe "and"
        (check-zero-flag-sets #(*and %1 :operand 0))
        (check-zero-flag-unsets #(*and (assoc %1 :a 1) :operand 1))
        (check-negative-flag-sets #(*and (assoc %1 :a 0x80) :operand 0x80))
        (check-negative-flag-unsets #(*and %1 :operand 0))

        (it "should and the argument with the accumulator"
          (let [new-cpu (*and (assoc cpu :a 0xff) :operand 0xa5)]
            (should= (:a new-cpu) 0xa5)))))

    (describe "sbc"
      (check-zero-flag-sets #(*sbc (assoc %1 :a 1) :operand 0))
      (check-zero-flag-unsets #(*sbc %1 :operand 1))
      (check-negative-flag-sets #(*sbc %1 :operand 0))
      (check-negative-flag-unsets #(*sbc %1 :operand 0xff))

      (it "should set the carry flag when the result is an unsigned underflow"
        (let [new-cpu (*sbc (assoc cpu :a 0) :operand 1)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result is not an unsigned underflow"
        (let [cpu-with-carry (set-flag (assoc cpu :a 1) carry-flag true)
              new-cpu (*sbc cpu-with-carry :operand 0)]
          (should-not (carry-flag? new-cpu))))

      (it "should set the overflow flag when subtracting a negative from a positive yields a negative"
        (let [cpu-with-carry (set-flag cpu carry-flag true)
              new-cpu (*sbc cpu-with-carry :operand 0x80)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when subtracting a positive from a negative yields a positive"
        (let [new-cpu (*sbc (assoc cpu :a 0x80) :operand 0x0f)]
          (should (overflow-flag? new-cpu))))

      (context "when the carry flag is clear"
        (it "should subtract the argument and an additional 1 from the accumulator"
          (let [new-cpu (*sbc (assoc cpu :a 0xff) :operand 0)]
            (should= (:a new-cpu) 0xfe))))

      (context "when the carry flag is set"
        (it "should subtract the argument from the accumulator"
          (let [new-cpu (*sbc (assoc cpu-with-carry :a 0xff) :operand 1)]
            (should= (:a new-cpu) 0xfe)))

        (it "should overflow if the subtraction would be less than 0"
          (let [new-cpu (*sbc cpu-with-carry :operand 1)]
            (should= (:a new-cpu) 0xff)))))

    (describe "adc"
      (it "should overflow if the addition would exceed 0xff"
        (let [new-cpu (*adc (assoc cpu :a 0xff) :operand 1)]
          (should= (:a new-cpu) 0)))

      (it "should set the carry flag when the result is an unsigned overflow"
        (let [new-cpu (*adc (assoc cpu :a 0xff) :operand 1)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result isn't an unsigned overflow"
        (let [new-cpu (*adc (set-flag cpu carry-flag true) :operand 1)]
          (should-not (carry-flag? new-cpu))))

      (check-zero-flag-sets #(*adc %1 :operand 0))
      (check-zero-flag-unsets #(*adc %1 :operand 1))
      (check-negative-flag-sets #(*adc %1 :operand 0x80))
      (check-negative-flag-unsets #(*adc %1 :operand 0))

      (it "should set the overflow flag when adding two positives yields a negative"
        (let [cpu (merge cpu {:a 0x79})
              new-cpu (*adc cpu :operand 0x79)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when adding two negatives yields a positive"
        (let [cpu (merge cpu {:a 0x80})
              new-cpu (*adc cpu :operand 0x80)]
          (should (overflow-flag? new-cpu))))

      (context "when the carry flag is clear"
        (it "should add the argument to the accumulator"
          (let [new-cpu (*adc cpu :operand 1)]
            (should= (:a new-cpu) 1))))

      (context "when the carry flag is set"
        (it "should add an additional 1 to the result"
          (let [new-cpu (*adc cpu-with-carry :operand 0)]
            (should= (:a new-cpu) 1)))

        (it "should unset the carry flag"
          (let [new-cpu (*adc cpu-with-carry :operand 1)]
            (should-not (carry-flag? new-cpu))))))))
