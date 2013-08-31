(ns clones.cpu-spec
  (:require [speclj.core :refer :all]
            [clones.cpu  :refer :all]))

(declare ^:dynamic cpu)
(describe "The NES CPU"
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

    (around [it]
       (binding [cpu (make-cpu)]
         (it)))

    (describe "register transfer operations"
      (map (fn [[op [from-reg to-reg]]]
             (describe (str op)
               (check-zero-flag-sets #((resolve op) (assoc %1 from-reg 0)))
               (check-zero-flag-unsets #((resolve op) (assoc %1 from-reg 1)))
               (check-negative-flag-sets #((resolve op) (assoc %1 from-reg 0x80)))
               (check-negative-flag-unsets #((resolve op) (assoc %1 from-reg 0)))

               (it (format "should transfer the value in the %s register to the %s register" (str from-reg) (str to-reg))
                 (let [new-cpu ((resolve op) (assoc cpu from-reg 0x44))]
                   (should= (to-reg new-cpu) 0x44)))))
        {'tax [:a :x]
         'tay [:a :y]
         'txa [:x :a]
         'tya [:y :a]
         'tsx [:sp :x]
         'txs [:x :sp]}))

    (describe "loading operations"
      (for [op ['lda 'ldx 'ldy]]
        (describe (name op)
          (check-zero-flag-sets #((resolve op) %1 0))
          (check-zero-flag-unsets #((resolve op) %1 1))
          (check-negative-flag-sets #((resolve op) %1 0x80))
          (check-negative-flag-unsets #((resolve op) %1 0))))

      (describe "ldy"
        (it "should load the y register with the argument"
          (let [new-cpu (ldy cpu 0xbb)]
            (should= (:y new-cpu) 0xbb))))

      (describe "ldx"
        (it "should load the x register with the argument"
          (let [new-cpu (ldx cpu 0xbb)]
            (should= (:x new-cpu) 0xbb))))

      (describe "lda"
        (it "should load the accumulator with the argument"
          (let [new-cpu (lda cpu 0xbb)]
            (should= (:a new-cpu) 0xbb)))))

    (describe "comparison operations"
      (map (fn [[op reg]]
        (describe (str op)
          (check-zero-flag-sets #((resolve op) %1 0))
          (check-zero-flag-unsets #((resolve op) (assoc %1 reg 1) 0))
          (check-negative-flag-sets #((resolve op) (assoc %1 reg 0x81) 1))
          (check-negative-flag-unsets #((resolve op) %1 0))

          (it (format "should set the carry flag if the %s register is greater than or equal to the argument" (str reg))
            (let [new-cpu ((resolve op) (assoc cpu reg 0x10) 1)]
              (should (carry-flag? new-cpu))))

          (it (format "should unset the carry flag if the %s register is less than the argument" (str reg))
            (let [cpu-with-carry (update-flags cpu overflow-flag)
                  new-cpu ((resolve op) cpu-with-carry 0x10)]
              (should-not (carry-flag? new-cpu))))))
        {'cmp :a
         'cpx :x
         'cpy :y}))

    (describe "logical operations"
      (describe "bit"
        (check-zero-flag-sets #(bit %1 0))
        (check-zero-flag-unsets #(bit (assoc %1 :a 1) 1))
        (check-negative-flag-sets #(bit (assoc %1 :a 0x80) 0x80))
        (check-negative-flag-unsets #(bit %1 0))

        (it "should set the overflow flag when the 6th bit of the result is set"
          (let [new-cpu (bit (assoc cpu :a 0x40) 0x40)]
            (should (overflow-flag? new-cpu))))

        (it "should unset the overflow flag when the 6th bit of the result is unset"
          (let [new-cpu (bit (update-flags cpu overflow-flag) 0)]
            (should-not (overflow-flag? new-cpu)))))

      (describe "eor"
        (check-zero-flag-sets #(eor %1 0))
        (check-zero-flag-unsets #(eor (assoc %1 :a 1) 0))
        (check-negative-flag-sets #(eor (assoc %1 :a 0x80) 0))
        (check-negative-flag-unsets #(eor %1 0)))

      (describe "ora"
        (check-zero-flag-sets #(ora %1 0))
        (check-zero-flag-unsets #(ora (assoc %1 :a 1) 1))
        (check-negative-flag-sets #(ora (assoc %1 :a 0x80) 0x80))
        (check-negative-flag-unsets #(ora %1 0))

        (it "should or the argument with the accumulator"
          (let [new-cpu (ora cpu 0xa5)]
            (should= (:a new-cpu) 0xa5))))

      (describe "and*"
        (check-zero-flag-sets #(and* %1 0))
        (check-zero-flag-unsets #(and* (assoc %1 :a 1) 1))
        (check-negative-flag-sets #(and* (assoc %1 :a 0x80) 0x80))
        (check-negative-flag-unsets #(and* %1 0))

        (it "should and the argument with the accumulator"
          (let [new-cpu (and* (assoc cpu :a 0xff) 0xa5)]
            (should= (:a new-cpu) 0xa5)))))

    (describe "sbc"
      (check-zero-flag-sets #(sbc (assoc %1 :a 1) 0))
      (check-zero-flag-unsets #(sbc %1 1))
      (check-negative-flag-sets #(sbc %1 0))
      (check-negative-flag-unsets #(sbc %1 0xff))

      (it "should set the carry flag when the result is an unsigned underflow"
        (let [new-cpu (sbc (assoc cpu :a 0) 1)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result is not an unsigned underflow"
        (let [cpu-with-carry (update-flags (assoc cpu :a 1) carry-flag)
              new-cpu (sbc cpu-with-carry 0)]
          (should-not (carry-flag? new-cpu))))

      (it "should set the overflow flag when subtracting a negative from a positive yields a negative"
        (let [cpu-with-carry (update-flags cpu carry-flag)
              new-cpu (sbc cpu-with-carry 0x80)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when subtracting a positive from a negative yields a positive"
        (let [new-cpu (sbc (assoc cpu :a 0x80) 0x0f)]
          (should (overflow-flag? new-cpu))))

      (context "when the carry flag is clear"
        (it "should subtract the argument and an additional 1 from the accumulator"
          (let [new-cpu (sbc (assoc cpu :a 0xff) 0)]
            (should= (:a new-cpu) 0xfe))))

      (context "when the carry flag is set"
        (around [it]
          (binding [cpu (assoc cpu :p carry-flag)]
            (it)))

        (it "should subtract the argument from the accumulator"
          (let [new-cpu (sbc (assoc cpu :a 0xff) 1)]
            (should= (:a new-cpu) 0xfe)))

        (it "should overflow if the subtraction would be less than 0"
          (let [new-cpu (sbc cpu 1)]
            (should= (:a new-cpu) 0xff)))))

    (describe "adc"
      (it "should overflow if the addition would exceed 0xff"
        (let [new-cpu (adc (assoc cpu :a 0xff) 1)]
          (should= (:a new-cpu) 0)))

      (it "should set the carry flag when the result is an unsigned overflow"
        (let [new-cpu (adc (assoc cpu :a 0xff) 1)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result isn't an unsigned overflow"
        (let [new-cpu (adc (update-flags cpu carry-flag) 1)]
          (should-not (carry-flag? new-cpu))))

      (check-zero-flag-sets #(adc %1 0))
      (check-zero-flag-unsets #(adc %1 1))
      (check-negative-flag-sets #(adc %1 0x80))
      (check-negative-flag-unsets #(adc %1 0))

      (it "should set the overflow flag when adding two positives yields a negative"
        (let [cpu (merge cpu {:a 0x79})
              new-cpu (adc cpu 0x79)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when adding two negatives yields a positive"
        (let [cpu (merge cpu {:a 0x80})
              new-cpu (adc cpu 0x80)]
          (should (overflow-flag? new-cpu))))

      (context "when the carry flag is clear"
        (it "should add the argument to the accumulator"
          (let [new-cpu (adc cpu 1)]
            (should= (:a new-cpu) 1))))

      (context "when the carry flag is set"
        (around [it]
          (binding [cpu (update-flags cpu carry-flag)]
            (it)))

        (it "should add an additional 1 to the result"
          (let [new-cpu (adc cpu 0)]
            (should= (:a new-cpu) 1)))

        (it "should unset the carry flag"
          (let [new-cpu (adc cpu 1)]
            (should-not (carry-flag? new-cpu))))))))
