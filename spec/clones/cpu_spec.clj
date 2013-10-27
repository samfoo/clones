(ns clones.cpu-spec
  (:require [speclj.core           :refer :all]
            [clones.cpu            :refer :all]
            [clones.cpu.memory     :refer :all]
            [clones.cpu.addressing :refer :all]))

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
        (should-not (carry-flag? (c (set-flag cpu carry-flag true))))))

    (defn check-carry-flag-sets [c]
      (it "should set the carry flag when the result carries"
        (should (carry-flag? (c cpu)))))

    (defn peek-stack-n [c n]
      (first
        (io-> c
              (io-read (+ 0x100 1 n (:sp c))))))

    (defn peek-stack [c] (peek-stack-n c 0))

    (defn with-stack-top [cpu v]
      (let [[_ after-write] (io-> cpu
                                  (io-write v 0x1fd))]
        (assoc after-write :sp 0xfc)))

    (defn check-branching [desc f should-branch should-not-branch]
      (it desc
        (should= 0 (:pc (f should-not-branch relative)))
        (let [[_ should-branch] (io-> should-branch
                                      (io-write 0x50 0))]
          ;; TODO: Not sure this is right? Why is 1 being added? Verify w/
          ;; nestest & output
          (should= 0x51 (:pc (f should-branch relative))))))

    (defn imm-n [cpu n]
      (second (io-> cpu (io-write n 0))))

    (describe "store operations"
      (describe "sty"
        (it "should store the y register in the address mode"
          (let [new-cpu ((op :sty) (assoc cpu :y 0xff) absolute)]
            (should= 0xff (first (io-> new-cpu
                                       (io-read 0)))))))

      (describe "stx"
        (it "should store the x register in the address mode"
          (let [new-cpu ((op :stx) (assoc cpu :x 0xff) absolute)]
            (should= 0xff (first (io-> new-cpu
                                       (io-read 0)))))))

      (describe "sta"
        (it "should store the accumulator in the address mode"
          (let [new-cpu ((op :sta) (assoc cpu :a 0xff) absolute)]
            (should= 0xff (first (io-> new-cpu
                                       (io-read 0))))))))

    (describe "shifts and rotates"
      (describe "ror"
        (check-zero-flag-sets #((op :ror) %1 accumulator))
        (check-zero-flag-unsets #((op :ror) (assoc %1 :a 0x80) accumulator))

        (check-negative-flag-sets #((op :ror) (assoc %1 :a 1) accumulator))
        (check-negative-flag-unsets #((op :ror) (assoc %1 :a 0x40) accumulator))

        (check-carry-flag-sets #((op :ror) (assoc %1 :a 1) accumulator))
        (check-carry-flag-unsets #((op :ror) (assoc %1 :a 0x80) accumulator))

        (it "should rotate the bits of the address mode right by 1"
          (let [cpu-rotated (-> cpu
                              (assoc :a 0x21)
                              ((op :ror) accumulator))]
            (should= 0x90 (:a cpu-rotated)))))

      (describe "rol"
        (check-zero-flag-sets #((op :rol) %1 accumulator))
        (check-zero-flag-unsets #((op :rol) (assoc %1 :a 0x80) accumulator))

        (check-negative-flag-sets #((op :rol) (assoc %1 :a 0x40) accumulator))
        (check-negative-flag-unsets #((op :rol) (assoc %1 :a 1) accumulator))

        (check-carry-flag-sets #((op :rol) (assoc %1 :a 0x80) accumulator))
        (check-carry-flag-unsets #((op :rol) (assoc %1 :a 1) accumulator))

        (it "should rotate the bits of the address mode left by 1"
          (let [cpu-rotated (-> cpu
                              (assoc :a 0x82)
                              ((op :rol) accumulator))]
            (should= 5 (:a cpu-rotated)))))

      (describe "lsr"
        (check-zero-flag-sets #((op :lsr) %1 accumulator))
        (check-zero-flag-unsets #((op :lsr) (assoc %1 :a 0x80) accumulator))

        (check-negative-flag-unsets #((op :lsr) (assoc %1 :a 1) accumulator))

        (check-carry-flag-sets #((op :lsr) (assoc %1 :a 1) accumulator))
        (check-carry-flag-unsets #((op :lsr) (assoc %1 :a 0x80) accumulator))

        (it "should shift the bits of the address mode right by 1"
          (let [cpu-shifted (-> cpu
                              (assoc :a 0x80)
                              ((op :lsr) accumulator))]
            (should= 0x40 (:a cpu-shifted)))))

      (describe "asl"
        (check-zero-flag-sets #((op :asl) %1 accumulator))
        (check-zero-flag-unsets #((op :asl) (assoc %1 :a 1) accumulator))

        (check-negative-flag-sets #((op :asl) (assoc %1 :a 0x40) accumulator))
        (check-negative-flag-unsets #((op :asl) (assoc %1 :a 1) accumulator))

        (check-carry-flag-sets #((op :asl) (assoc %1 :a 0x80) accumulator))
        (check-carry-flag-unsets #((op :asl) (assoc %1 :a 1) accumulator))

        (it "should shift the bits of the address mode left by 1"
          (let [cpu-shifted (-> cpu
                              (assoc :a 1)
                              ((op :asl) accumulator))]
            (should= 2 (:a cpu-shifted))))))

    (describe "system functions"
      (let [cpu (io-mount cpu 0xfffa 0xffff {})]
        (describe "brk"
          (it "should set the program counter to the value at 0xfffe (the IRQ/BRK vector)"
              (let [[_ cpu-with-vector] (io-> cpu
                                              (io-write 0xff 0xffff)
                                              (io-write 0xee 0xfffe))
                    new-cpu ((op :brk) cpu-with-vector implied)]
              (should= 0xffee (:pc new-cpu))))

          (it "should push the current program counter (plus 1) to the stack"
            (let [new-cpu ((op :brk) (assoc cpu :pc 0xbeef) implied)
                  low (peek-stack-n new-cpu 1)
                  high (peek-stack-n new-cpu 2)]
              (should= 0xbe high)
              (should= 0xf0 low)))

          (it "should push processor flags to the stack with the interrupt flag set"
            (let [new-cpu ((op :brk) (assoc cpu :p 0) implied)
                  stack-top (peek-stack new-cpu)]
              (should= 0x10 stack-top)))))

      (describe "nop"
        (it "should do nothing"
          (should= cpu ((op :nop) cpu implied)))))

    (describe "status flag changes"
      (describe "sei"
        (it "should set the interrupt flag"
          (should= true (interrupt-flag? ((op :sei) cpu implied)))))

      (describe "sed"
        (it "should set the decimal flag"
          (should= true (decimal-flag? ((op :sed) cpu implied)))))

      (describe "sec"
        (it "should set the carry flag"
          (should= true (carry-flag? ((op :sec) cpu implied)))))

      (describe "clv"
        (it "should clear the overflow flag"
          (should= false (overflow-flag? ((op :clv) cpu-with-overflow implied)))))

      (describe "cli"
        (it "should clear the interrupt flag"
          (should= false (interrupt-flag? ((op :cli) cpu-with-interrupt implied)))))

      (describe "cld"
        (it "should clear the decimal flag"
          (should= false (decimal-flag? ((op :cld) cpu-with-decimal implied)))))

      (describe "clc"
        (it "should clear the carry flag"
          (should= false (carry-flag? ((op :clc) cpu-with-carry implied))))))

    (describe "branching"
      (describe "bvs"
        (check-branching "should branch when overflow set"
                         (op :bvs)
                         cpu-with-overflow
                         cpu))
      (describe "bvc"
        (check-branching "should branch when overflow clear"
                         (op :bvc)
                         cpu
                         cpu-with-overflow))
      (describe "bpl"
        (check-branching "should branch when negative clear"
                         (op :bpl)
                         cpu
                         cpu-with-negative))
      (describe "bne"
        (check-branching "should branch when zero clear"
                         (op :bne)
                         cpu
                         cpu-with-zero))
      (describe "bmi"
        (check-branching "should branch when negative set"
                         (op :bmi)
                         cpu-with-negative
                         cpu))
      (describe "beq"
        (check-branching "should branch when zero set"
                         (op :beq)
                         cpu-with-zero
                         cpu))
      (describe "bcs"
        (check-branching "should branch when carry set"
                         (op :bcs)
                         cpu-with-carry
                         cpu))
      (describe "bcc"
        (check-branching "should branch when carry clear"
                         (op :bcc)
                         cpu
                         cpu-with-carry)))

    (describe "jumps and calls"
      (describe "rts"
        (it "should pull the program counter from the stack and then add one to it"
          (let [cpu-with-pc (-> cpu
                              (stack-push 0xff)
                              (stack-push 0xdd))
                new-cpu ((op :rts) cpu-with-pc implied)]
            (should= 0xffde (:pc new-cpu)))))

      (describe "rti"
        (it "should pull the program counter from the stack after pulling flags"
          (let [cpu-with-p-and-pc (-> cpu
                                    (stack-push 0xff)
                                    (stack-push 0xdd)
                                    (stack-push 0))
                new-cpu ((op :rti) cpu-with-p-and-pc implied)]
            (should= 0xffdd (:pc new-cpu))))

        (it "should pull the flags from the top of the stack (break flag is always 0)"
          (let [new-cpu ((op :rti) (with-stack-top cpu 0xff) implied)]
            (should= 0xef (:p new-cpu)))))

      (describe "jsr"
        (it "should set the program counter to the operand"
          (let [with-jsr-addr (second (io-> cpu
                                            (io-write-word 0xbeef 0)))
                new-cpu ((op :jsr) with-jsr-addr absolute)]
            (should= 0xbeef (:pc new-cpu))))

        (it "should push the current program counter (minus 1) to the stack"
          (let [cpu (io-mount cpu 0x2000 0xffff {})
                new-cpu ((op :jsr) (assoc cpu :pc 0xffdd) absolute)]
            (should= 0xdc (peek-stack-n new-cpu 0))
            (should= 0xff (peek-stack-n new-cpu 1)))))

      (describe "jmp"
        (it "should set the program counter to the argument"
          (let [with-jmp-addr (second (io-> cpu
                                            (io-write-word 0x1000 0)))
                new-cpu ((op :jmp) with-jmp-addr absolute)]
            (should= 0x1000 (:pc new-cpu))))))

    (describe "stack operations"
      (describe "plp"
        (it "should pull the 5th bit as 1, no matter what"
          (let [new-cpu ((op :plp) (with-stack-top cpu 0x00) implied)]
            (should= 0x20 (:p new-cpu))))

        (it "should pull the break flag as 0, no matter what"
          (let [new-cpu ((op :plp) (with-stack-top cpu 0xff) implied)]
            (should= 0xef (:p new-cpu))))

        (it "should pull the top of the stack into the flags register"
          (let [new-cpu ((op :plp) (with-stack-top cpu 0x21) implied)]
            (should= 0x21 (:p new-cpu)))))

      (describe "pla"
        (check-zero-flag-sets #((op :pla) (with-stack-top % 0x00) implied))
        (check-zero-flag-unsets #((op :pla) (with-stack-top % 0x01) implied))
        (check-negative-flag-sets #((op :pla) (with-stack-top % 0x80) implied))
        (check-negative-flag-unsets #((op :pla) (with-stack-top % 0x00) implied))

        (it "should pull the top of the stack into the accumulator"
          (let [new-cpu ((op :pla) (with-stack-top cpu 0xff) implied)]
            (should= 0xff (:a new-cpu)))))

      (describe "php"
        (it "should always push the break flag as 1"
          ;; NOTE: This is only in the NES's 6502... should there be a way of
          ;; setting it so that the CPU can behave either way?
          (let [new-cpu ((op :php) (assoc cpu :p 0) implied)
                stack-top (peek-stack new-cpu)]
            (should= 0 (:p new-cpu))
            (should= break-flag stack-top)))

        (it "should push processor flags to the stack"
          (let [new-cpu ((op :php) cpu-with-carry implied)
                stack-top (peek-stack new-cpu)]
            (should= (bit-or carry-flag break-flag) stack-top))))

      (describe "pha"
        (it "should wrap the stack pointer to 0xff if it is 0x00"
          (let [cpu-with-0-sp (assoc cpu :sp 0x00)
                new-cpu ((op :pha) cpu-with-0-sp implied)]
            (should= 0xff (:sp new-cpu))))

        (it "should decrement the stack pointer"
          (let [new-cpu ((op :pha) (assoc cpu :a 0xbe) implied)
                new-sp (:sp new-cpu)]
            (should= (dec (:sp cpu)) (:sp new-cpu))))

        (it "should push the accumulator to the stack"
          (let [new-cpu ((op :pha) (assoc cpu :a 0xbe) implied)
                stack-top (peek-stack new-cpu)]
            (should= 0xbe stack-top)))))

    (describe "decrement operations"
      (describe "dec"
        (check-zero-flag-sets #((op :dec) (second (io-> %1 (io-write 1 0))) immediate))
        (check-zero-flag-unsets #((op :dec) %1 immediate))
        (check-negative-flag-sets #((op :dec) %1 immediate))
        (check-negative-flag-unsets #((op :dec) (second (io-> %1 (io-write 1 0))) immediate))

        (it "should decrement the value at the address mode by one"
          (let [new-cpu ((op :dec) cpu immediate)]
            (should= 0xff (first (io-> new-cpu
                                       (io-read 0)))))))

      (map (fn [[op reg]]
             (describe (str op)
               (check-zero-flag-sets #(op (assoc %1 reg 1) implied))
               (check-zero-flag-unsets #(op (assoc %1 reg 2) implied))
               (check-negative-flag-sets #(op (assoc %1 reg 0) implied))
               (check-negative-flag-unsets #(op (assoc %1 reg 1) implied))

               (it (format "should decrement the %s register by one" (str reg))
                 (let [new-cpu (op cpu implied)]
                   (should= 0xff (reg new-cpu))))))
        {(op :dex) :x
         (op :dey) :y}))

    (describe "increment operations"
      (describe "inc"
        (check-zero-flag-sets #((op :inc) (second (io-> %1 (io-write 0xff 0))) immediate))
        (check-zero-flag-unsets #((op :inc) %1 immediate))
        (check-negative-flag-sets #((op :inc) (second (io-> %1 (io-write 0x7f 0))) immediate))
        (check-negative-flag-unsets #((op :inc) %1 immediate))

        (it "should increment the value at the address mode by one"
          (let [new-cpu ((op :inc) cpu immediate)]
            (should= 1 (first (io-> new-cpu
                                    (io-read 0)))))))

      (map (fn [[op reg]]
             (describe (str op)
               (check-zero-flag-sets #(op (assoc %1 reg 0xff) implied))
               (check-zero-flag-unsets #(op (assoc %1 reg 0) implied))
               (check-negative-flag-sets #(op (assoc %1 reg 0x7f) implied))
               (check-negative-flag-unsets #(op (assoc %1 reg 0) implied))

               (it (format "should increment the %s register by one" (str reg))
                 (let [new-cpu (op cpu implied)]
                   (should= 1 (reg new-cpu))))))
        {(op :inx) :x
         (op :iny) :y}))

    (describe "register transfer operations"
      (map (fn [[op [from-reg to-reg]]]
             (describe (str op)
               (check-zero-flag-sets #(op (assoc %1 from-reg 0) implied))
               (check-zero-flag-unsets #(op (assoc %1 from-reg 1) implied))
               (check-negative-flag-sets #(op (assoc %1 from-reg 0x80) implied))
               (check-negative-flag-unsets #(op (assoc %1 from-reg 0) implied))

               (it (format "should transfer the value in the %s register to the %s register" (str from-reg) (str to-reg))
                 (let [new-cpu (op (assoc cpu from-reg 0x44) implied)]
                   (should= (to-reg new-cpu) 0x44)))))
        {(op :tax) [:a :x]
         (op :tay) [:a :y]
         (op :txa) [:x :a]
         (op :tya) [:y :a]
         (op :tsx) [:sp :x]
         (op :txs) [:x :sp]}))

    (describe "loading operations"
      (for [op [(op :lda) (op :ldx) (op :ldy)]]
        (describe (str op)
          (check-zero-flag-sets #(op %1 immediate))
          (check-zero-flag-unsets #(op (imm-n %1 1) immediate))
          (check-negative-flag-sets #(op (imm-n %1 0x80) immediate))
          (check-negative-flag-unsets #(op %1 immediate))))

      (describe "ldy"
        (it "should load the y register with the argument"
          (let [new-cpu ((op :ldy) (imm-n cpu 0xbb) immediate)]
            (should= (:y new-cpu) 0xbb))))

      (describe "ldx"
        (it "should load the x register with the argument"
          (let [new-cpu ((op :ldx) (imm-n cpu 0xbb) immediate)]
            (should= (:x new-cpu) 0xbb))))

      (describe "lda"
        (it "should load the accumulator with the argument"
          (let [new-cpu ((op :lda) (imm-n cpu 0xbb) immediate)]
            (should= (:a new-cpu) 0xbb)))))

    (describe "comparison operations"
      (def cpu-with-imm-1 (second (io-> cpu
                                        (io-write 1 0))))

      (map (fn [[op reg]]
        (describe (str op)
          (check-zero-flag-sets #(op %1 immediate))
          (check-zero-flag-unsets #(op (assoc %1 reg 1) immediate))
          (check-negative-flag-sets (fn [c]
                                      (op (assoc c reg 0x81)
                                          immediate)))
          (check-negative-flag-unsets #(op %1 immediate))

          (it (format "should set the carry flag if the %s register is greater than or equal to the operand" (str reg))
            (let [new-cpu (op (assoc cpu-with-imm-1 reg 0x10) immediate)]
              (should (carry-flag? new-cpu))))

          (it (format "should unset the carry flag if the %s register is less than the operand" (str reg))
            (let [cpu-with-carry (set-flag cpu-with-imm-1 overflow-flag true)
                  new-cpu (op cpu-with-carry immediate)]
              (should-not (carry-flag? new-cpu))))))
        {(op :cmp) :a
         (op :cpx) :x
         (op :cpy) :y}))

    (describe "logical operations"
      (describe "bit"
        (check-zero-flag-sets #((op :bit) %1 immediate))
        (check-zero-flag-unsets #((op :bit) (imm-n (assoc %1 :a 1) 1) immediate))
        (check-negative-flag-sets #((op :bit) (imm-n (assoc %1 :a 0x80) 0x80) immediate))
        (check-negative-flag-unsets #((op :bit) %1 immediate))

        (it "should set the overflow flag when the 6th bit of the result is set"
          (let [new-cpu ((op :bit) (imm-n (assoc cpu :a 0x40) 0x40) immediate)]
            (should (overflow-flag? new-cpu))))

        (it "should unset the overflow flag when the 6th bit of the result is unset"
          (let [new-cpu ((op :bit) (set-flag cpu overflow-flag true) immediate)]
            (should-not (overflow-flag? new-cpu)))))

      (describe "eor"
        (check-zero-flag-sets #((op :eor) %1 immediate))
        (check-zero-flag-unsets #((op :eor) (assoc %1 :a 1) immediate))
        (check-negative-flag-sets #((op :eor) (assoc %1 :a 0x80) immediate))
        (check-negative-flag-unsets #((op :eor) %1 immediate)))

      (describe "ora"
        (check-zero-flag-sets #((op :ora) %1 immediate))
        (check-zero-flag-unsets #((op :ora) (imm-n (assoc %1 :a 1) 1) immediate))
        (check-negative-flag-sets #((op :ora) (imm-n (assoc %1 :a 0x80) 0x80) immediate))
        (check-negative-flag-unsets #((op :ora) %1 immediate))

        (it "should or the argument with the accumulator"
          (let [new-cpu ((op :ora) (imm-n cpu 0xa5) immediate)]
            (should= (:a new-cpu) 0xa5))))

      (describe "and"
        (check-zero-flag-sets #((op :and) %1 immediate))
        (check-zero-flag-unsets #((op :and) (imm-n (assoc %1 :a 1) 1) immediate))
        (check-negative-flag-sets #((op :and) (imm-n (assoc %1 :a 0x80) 0x80) immediate))
        (check-negative-flag-unsets #((op :and) %1 immediate))

        (it "should and the argument with the accumulator"
          (let [new-cpu ((op :and) (imm-n (assoc cpu :a 0xff) 0xa5) immediate)]
            (should= (:a new-cpu) 0xa5)))))

    (describe "sbc"
      (check-zero-flag-sets #((op :sbc) (assoc %1 :a 1) immediate))
      (check-zero-flag-unsets #((op :sbc) (imm-n %1 1) immediate))
      (check-negative-flag-sets #((op :sbc) %1 immediate))
      (check-negative-flag-unsets #((op :sbc) (imm-n %1 0xff) immediate))

      (it "should set the carry flag when the result is an unsigned underflow"
        (let [new-cpu ((op :sbc) (imm-n (assoc cpu :a 0) 1) immediate)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result is not an unsigned underflow"
        (let [cpu-with-carry (set-flag (assoc cpu :a 1) carry-flag true)
              new-cpu ((op :sbc) cpu-with-carry immediate)]
          (should-not (carry-flag? new-cpu))))

      (it "should set the overflow flag when subtracting a negative from a positive yields a negative"
        (let [cpu-with-carry (set-flag cpu carry-flag true)
              new-cpu ((op :sbc) (imm-n cpu-with-carry 0x80) immediate)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when subtracting a positive from a negative yields a positive"
        (let [new-cpu ((op :sbc) (imm-n (assoc cpu :a 0x80) 0x0f) immediate)]
          (should (overflow-flag? new-cpu))))

      (context "when the carry flag is clear"
        (it "should subtract the argument and an additional 1 from the accumulator"
          (let [new-cpu ((op :sbc) (assoc cpu :a 0xff) immediate)]
            (should= (:a new-cpu) 0xfe))))

      (context "when the carry flag is set"
        (it "should subtract the argument from the accumulator"
          (let [new-cpu ((op :sbc) (imm-n (assoc cpu-with-carry :a 0xff) 1) immediate)]
            (should= (:a new-cpu) 0xfe)))

        (it "should overflow if the subtraction would be less than 0"
          (let [new-cpu ((op :sbc) (imm-n cpu-with-carry 1) immediate)]
            (should= (:a new-cpu) 0xff)))))

    (describe "adc"
      (it "should overflow if the addition would exceed 0xff"
        (let [new-cpu ((op :adc) (assoc (imm-n cpu 1) :a 0xff) immediate)]
          (should= (:a new-cpu) 0)))

      (it "should set the carry flag when the result is an unsigned overflow"
        (let [new-cpu ((op :adc) (assoc (imm-n cpu 1) :a 0xff) immediate)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result isn't an unsigned overflow"
        (let [new-cpu ((op :adc) (set-flag (imm-n cpu 1) carry-flag true) immediate)]
          (should-not (carry-flag? new-cpu))))

      (check-zero-flag-sets #((op :adc) (imm-n %1 0) immediate))
      (check-zero-flag-unsets #((op :adc) (imm-n %1 1) immediate))
      (check-negative-flag-sets #((op :adc) (imm-n %1 0x80) immediate))
      (check-negative-flag-unsets #((op :adc) (imm-n %1 0) immediate))

      (it "should set the overflow flag when adding two positives yields a negative"
        (let [cpu (merge cpu {:a 0x79})
              new-cpu ((op :adc) (imm-n cpu 0x79) immediate)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when adding two negatives yields a positive"
        (let [cpu (merge cpu {:a 0x80})
              new-cpu ((op :adc) (imm-n cpu 0x80) immediate)]
          (should (overflow-flag? new-cpu))))

      (context "when the carry flag is clear"
        (it "should add the argument to the accumulator"
          (let [new-cpu ((op :adc) (imm-n cpu 1) immediate)]
            (should= (:a new-cpu) 1))))

      (context "when the carry flag is set"
        (it "should add an additional 1 to the result"
          (let [new-cpu ((op :adc) cpu-with-carry immediate)]
            (should= (:a new-cpu) 1)))

        (it "should unset the carry flag"
          (let [new-cpu ((op :adc) (imm-n cpu-with-carry 1) immediate)]
            (should-not (carry-flag? new-cpu))))))))
