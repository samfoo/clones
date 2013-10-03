(ns clones.cpu-spec
  (:require [speclj.core   :refer :all]
            [clones.cpu    :refer :all]
            [clones.memory :refer :all]))

(def cpu (make-cpu))
(def cpu-with-carry (assoc cpu :p carry-flag))

(describe "A 6502 CPU"
  (describe "instruction set"
    (defn check-zero-flag-sets [c]
      (it "should set the zero flag when the result is zero"
        (should (zero-flag? (c cpu)))))

    (defn check-zero-flag-unsets [c]
      (it "should unset the zero flag when the result is non-zero"
        (should-not (zero-flag? (c (update-flags cpu zero-flag))))))

    (defn check-negative-flag-sets [c]
      (it "should set the negative flag when the result is negative"
        (should (negative-flag? (c cpu)))))

    (defn check-negative-flag-unsets [c]
      (it "should unset the negative flag when the result is non-negative"
        (should-not (negative-flag? (c (update-flags cpu negative-flag))))))

    (defn peek-stack-n [c n]
      (mount-read (:memory c) (+ 0x100 1 n (:sp c))))

    (defn peek-stack [c] (peek-stack-n c 0))

  (describe "jumps and calls"
    (describe "jsr"
      (it "should set the program counter to the argument"
        (let [new-cpu (*asm-jsr cpu 0xbeef)]
          (should= 0xbeef (:pc new-cpu))))

      (it "should push the current program counter (minus 1) to the stack"
        (let [new-cpu (*asm-jsr (assoc cpu :pc 0xffdd) 0)]
          (should= 0xdc (peek-stack-n new-cpu 0))
          (should= 0xff (peek-stack-n new-cpu 1)))))

    (describe "jmp"
      (it "should set the program counter to the argument"
        (let [new-cpu (*asm-jmp cpu 0x1000)]
          (should= 0x1000 (:pc new-cpu))))))

  (describe "stack operations"
    (defn with-stack-top [cpu v]
      (let [stack-with-value (mount-write (:memory cpu) v 0x1fd)]
        (merge cpu {:memory stack-with-value :sp 0x1fc})))

    (describe "plp"
      (it "should pull the 5th bit as 1, no matter what"
        (let [new-cpu (*asm-plp (with-stack-top cpu 0x00) nil)]
          (should= 0x20 (:p new-cpu))))

      (it "should pull the break flag as 0, no matter what"
        (let [new-cpu (*asm-plp (with-stack-top cpu 0xff) nil)]
          (should= 0xef (:p new-cpu))))

      (it "should pull the top of the stack into the flags register"
        (let [new-cpu (*asm-plp (with-stack-top cpu 0x21) nil)]
          (should= 0x21 (:p new-cpu)))))

    (describe "pla"
      (check-zero-flag-sets #(*asm-pla (with-stack-top % 0x00) nil))
      (check-zero-flag-unsets #(*asm-pla (with-stack-top % 0x01) nil))
      (check-negative-flag-sets #(*asm-pla (with-stack-top % 0x80) nil))
      (check-negative-flag-unsets #(*asm-pla (with-stack-top % 0x00) nil))

      (it "should pull the top of the stack into the accumulator"
        (let [new-cpu (*asm-pla (with-stack-top cpu 0xff) nil)]
          (should= 0xff (:a new-cpu)))))

    (describe "php"
      (it "should always push the break flag as 1"
        ;; NOTE: This is only in the NES's 6502... should there be a way of
        ;; setting it so that the CPU can behave either way?
        (let [new-cpu (*asm-php cpu nil)
              stack-top (peek-stack new-cpu)]
          (should= 0 (:p cpu))
          (should= break-flag stack-top)))

      (it "should push processor flags to the stack"
        (let [new-cpu (*asm-php cpu-with-carry nil)
              stack-top (peek-stack new-cpu)]
          (should= (bit-or carry-flag break-flag) stack-top))))

    (describe "pha"
      (it "should wrap the stack pointer to 0xff if it is 0x00"
        (let [cpu-with-0-sp (assoc cpu :sp 0x00)
              new-cpu (*asm-pha cpu-with-0-sp nil)]
          (should= 0xff (:sp new-cpu))))

      (it "should decrement the stack pointer"
        (let [new-cpu (*asm-pha (assoc cpu :a 0xbe) nil)
              new-sp (:sp new-cpu)]
          (should= (dec (:sp cpu)) (:sp new-cpu))))

      (it "should push the accumulator to the stack"
        (let [new-cpu (*asm-pha (assoc cpu :a 0xbe) nil)
              stack-top (peek-stack new-cpu)]
          (should= 0xbe stack-top)))))

   (describe "decrement operations"
     (map (fn [[op reg]]
            (describe (str op)
              (check-zero-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 reg 1) nil))
              (check-zero-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 reg 2) nil))
              (check-negative-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 reg 0) nil))
              (check-negative-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 reg 1) nil))

              (it (format "should decrement the %s register by one" (str reg))
                (let [new-cpu ((ns-resolve 'clones.cpu op) cpu nil)]
                  (should= 0xff (reg new-cpu))))))
       {'*asm-dec :a
        '*asm-dex :x
        '*asm-dey :y}))

   (describe "increment operations"
     (map (fn [[op reg]]
            (describe (str op)
              (check-zero-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 reg 0xff) nil))
              (check-zero-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 reg 0) nil))
              (check-negative-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 reg 0x7f) nil))
              (check-negative-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 reg 0) nil))

              (it (format "should increment the %s register by one" (str reg))
                (let [new-cpu ((ns-resolve 'clones.cpu op) cpu nil)]
                  (should= 1 (reg new-cpu))))))
       {'*asm-inc :a
        '*asm-inx :x
        '*asm-iny :y}))

   (describe "register transfer operations"
     (map (fn [[op [from-reg to-reg]]]
            (describe (str op)
              (check-zero-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 from-reg 0) nil))
              (check-zero-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 from-reg 1) nil))
              (check-negative-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 from-reg 0x80) nil))
              (check-negative-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 from-reg 0) nil))

              (it (format "should transfer the value in the %s register to the %s register" (str from-reg) (str to-reg))
                (let [new-cpu ((ns-resolve 'clones.cpu op) (assoc cpu from-reg 0x44) nil)]
                  (should= (to-reg new-cpu) 0x44)))))
       {'*asm-tax [:a :x]
        '*asm-tay [:a :y]
        '*asm-txa [:x :a]
        '*asm-tya [:y :a]
        '*asm-tsx [:sp :x]
        '*asm-txs [:x :sp]}))

   (describe "loading operations"
     (for [op ['*asm-lda '*asm-ldx '*asm-ldy]]
       (describe (name op)
         (check-zero-flag-sets #((ns-resolve 'clones.cpu op) %1 0))
         (check-zero-flag-unsets #((ns-resolve 'clones.cpu op) %1 1))
         (check-negative-flag-sets #((ns-resolve 'clones.cpu op) %1 0x80))
         (check-negative-flag-unsets #((ns-resolve 'clones.cpu op) %1 0))))

     (describe "ldy"
       (it "should load the y register with the argument"
         (let [new-cpu (*asm-ldy cpu 0xbb)]
           (should= (:y new-cpu) 0xbb))))

     (describe "ldx"
       (it "should load the x register with the argument"
         (let [new-cpu (*asm-ldx cpu 0xbb)]
           (should= (:x new-cpu) 0xbb))))

     (describe "lda"
       (it "should load the accumulator with the argument"
         (let [new-cpu (*asm-lda cpu 0xbb)]
           (should= (:a new-cpu) 0xbb)))))

   (describe "comparison operations"
     (map (fn [[op reg]]
       (describe (str op)
         (check-zero-flag-sets #((ns-resolve 'clones.cpu op) %1 0))
         (check-zero-flag-unsets #((ns-resolve 'clones.cpu op) (assoc %1 reg 1) 0))
         (check-negative-flag-sets #((ns-resolve 'clones.cpu op) (assoc %1 reg 0x81) 1))
         (check-negative-flag-unsets #((ns-resolve 'clones.cpu op) %1 0))

         (it (format "should set the carry flag if the %s register is greater than or equal to the argument" (str reg))
           (let [new-cpu ((ns-resolve 'clones.cpu op) (assoc cpu reg 0x10) 1)]
             (should (carry-flag? new-cpu))))

         (it (format "should unset the carry flag if the %s register is less than the argument" (str reg))
           (let [cpu-with-carry (update-flags cpu overflow-flag)
                 new-cpu ((ns-resolve 'clones.cpu op) cpu-with-carry 0x10)]
             (should-not (carry-flag? new-cpu))))))
       {'*asm-cmp :a
        '*asm-cpx :x
        '*asm-cpy :y}))

   (describe "logical operations"
     (describe "bit"
       (check-zero-flag-sets #(*asm-bit %1 0))
       (check-zero-flag-unsets #(*asm-bit (assoc %1 :a 1) 1))
       (check-negative-flag-sets #(*asm-bit (assoc %1 :a 0x80) 0x80))
       (check-negative-flag-unsets #(*asm-bit %1 0))

       (it "should set the overflow flag when the 6th bit of the result is set"
         (let [new-cpu (*asm-bit (assoc cpu :a 0x40) 0x40)]
           (should (overflow-flag? new-cpu))))

       (it "should unset the overflow flag when the 6th bit of the result is unset"
         (let [new-cpu (*asm-bit (update-flags cpu overflow-flag) 0)]
           (should-not (overflow-flag? new-cpu)))))

     (describe "eor"
       (check-zero-flag-sets #(*asm-eor %1 0))
       (check-zero-flag-unsets #(*asm-eor (assoc %1 :a 1) 0))
       (check-negative-flag-sets #(*asm-eor (assoc %1 :a 0x80) 0))
       (check-negative-flag-unsets #(*asm-eor %1 0)))

     (describe "ora"
       (check-zero-flag-sets #(*asm-ora %1 0))
       (check-zero-flag-unsets #(*asm-ora (assoc %1 :a 1) 1))
       (check-negative-flag-sets #(*asm-ora (assoc %1 :a 0x80) 0x80))
       (check-negative-flag-unsets #(*asm-ora %1 0))

       (it "should or the argument with the accumulator"
         (let [new-cpu (*asm-ora cpu 0xa5)]
           (should= (:a new-cpu) 0xa5))))

     (describe "and"
       (check-zero-flag-sets #(*asm-and %1 0))
       (check-zero-flag-unsets #(*asm-and (assoc %1 :a 1) 1))
       (check-negative-flag-sets #(*asm-and (assoc %1 :a 0x80) 0x80))
       (check-negative-flag-unsets #(*asm-and %1 0))

       (it "should and the argument with the accumulator"
         (let [new-cpu (*asm-and (assoc cpu :a 0xff) 0xa5)]
           (should= (:a new-cpu) 0xa5)))))

   (describe "sbc"
     (check-zero-flag-sets #(*asm-sbc (assoc %1 :a 1) 0))
     (check-zero-flag-unsets #(*asm-sbc %1 1))
     (check-negative-flag-sets #(*asm-sbc %1 0))
     (check-negative-flag-unsets #(*asm-sbc %1 0xff))

     (it "should set the carry flag when the result is an unsigned underflow"
       (let [new-cpu (*asm-sbc (assoc cpu :a 0) 1)]
         (should (carry-flag? new-cpu))))

     (it "should unset the carry flag when the result is not an unsigned underflow"
       (let [cpu-with-carry (update-flags (assoc cpu :a 1) carry-flag)
             new-cpu (*asm-sbc cpu-with-carry 0)]
         (should-not (carry-flag? new-cpu))))

     (it "should set the overflow flag when subtracting a negative from a positive yields a negative"
       (let [cpu-with-carry (update-flags cpu carry-flag)
             new-cpu (*asm-sbc cpu-with-carry 0x80)]
         (should (overflow-flag? new-cpu))))

     (it "should set the overflow flag when subtracting a positive from a negative yields a positive"
       (let [new-cpu (*asm-sbc (assoc cpu :a 0x80) 0x0f)]
         (should (overflow-flag? new-cpu))))

     (context "when the carry flag is clear"
       (it "should subtract the argument and an additional 1 from the accumulator"
         (let [new-cpu (*asm-sbc (assoc cpu :a 0xff) 0)]
           (should= (:a new-cpu) 0xfe))))

     (context "when the carry flag is set"
       (it "should subtract the argument from the accumulator"
         (let [new-cpu (*asm-sbc (assoc cpu-with-carry :a 0xff) 1)]
           (should= (:a new-cpu) 0xfe)))

       (it "should overflow if the subtraction would be less than 0"
         (let [new-cpu (*asm-sbc cpu-with-carry 1)]
           (should= (:a new-cpu) 0xff)))))

   (describe "adc"
     (it "should overflow if the addition would exceed 0xff"
       (let [new-cpu (*asm-adc (assoc cpu :a 0xff) 1)]
         (should= (:a new-cpu) 0)))

     (it "should set the carry flag when the result is an unsigned overflow"
       (let [new-cpu (*asm-adc (assoc cpu :a 0xff) 1)]
         (should (carry-flag? new-cpu))))

     (it "should unset the carry flag when the result isn't an unsigned overflow"
       (let [new-cpu (*asm-adc (update-flags cpu carry-flag) 1)]
         (should-not (carry-flag? new-cpu))))

     (check-zero-flag-sets #(*asm-adc %1 0))
     (check-zero-flag-unsets #(*asm-adc %1 1))
     (check-negative-flag-sets #(*asm-adc %1 0x80))
     (check-negative-flag-unsets #(*asm-adc %1 0))

     (it "should set the overflow flag when adding two positives yields a negative"
       (let [cpu (merge cpu {:a 0x79})
             new-cpu (*asm-adc cpu 0x79)]
         (should (overflow-flag? new-cpu))))

     (it "should set the overflow flag when adding two negatives yields a positive"
       (let [cpu (merge cpu {:a 0x80})
             new-cpu (*asm-adc cpu 0x80)]
         (should (overflow-flag? new-cpu))))

     (context "when the carry flag is clear"
       (it "should add the argument to the accumulator"
         (let [new-cpu (*asm-adc cpu 1)]
           (should= (:a new-cpu) 1))))

     (context "when the carry flag is set"
       (it "should add an additional 1 to the result"
         (let [new-cpu (*asm-adc cpu-with-carry 0)]
           (should= (:a new-cpu) 1)))

       (it "should unset the carry flag"
         (let [new-cpu (*asm-adc cpu-with-carry 1)]
           (should-not (carry-flag? new-cpu))))))))
