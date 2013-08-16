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

    (describe "register transfers"
      (describe "txs"
        (check-zero-flag-sets #(txs (assoc %1 :x 0)))
        (check-zero-flag-unsets #(txs (assoc %1 :x 1)))
        (check-negative-flag-sets #(txs (assoc %1 :x 0x80)))
        (check-negative-flag-unsets #(txs (assoc %1 :x 0)))

        (it "should transfer the value in the x register to the accumulator"
          (let [new-cpu (txs (assoc cpu :x 0x44))]
            (should= (:sp new-cpu) 0x44))))

      (describe "tsx"
        (check-zero-flag-sets #(tsx (assoc %1 :sp 0)))
        (check-zero-flag-unsets #(tsx (assoc %1 :sp 1)))
        (check-negative-flag-sets #(tsx (assoc %1 :sp 0x80)))
        (check-negative-flag-unsets #(tsx (assoc %1 :sp 0)))

        (it "should transfer the value in the x register to the accumulator"
          (let [new-cpu (tsx (assoc cpu :sp 0x44))]
            (should= (:x new-cpu) 0x44))))

      (describe "tya"
        (check-zero-flag-sets #(tya (assoc %1 :y 0)))
        (check-zero-flag-unsets #(tya (assoc %1 :y 1)))
        (check-negative-flag-sets #(tya (assoc %1 :y 0x80)))
        (check-negative-flag-unsets #(tya (assoc %1 :y 0)))

        (it "should transfer the value in the x register to the accumulator"
          (let [new-cpu (tya (assoc cpu :y 0x44))]
            (should= (:a new-cpu) 0x44))))

      (describe "txa"
        (check-zero-flag-sets #(txa (assoc %1 :x 0)))
        (check-zero-flag-unsets #(txa (assoc %1 :x 1)))
        (check-negative-flag-sets #(txa (assoc %1 :x 0x80)))
        (check-negative-flag-unsets #(txa (assoc %1 :x 0)))

        (it "should transfer the value in the x register to the accumulator"
          (let [new-cpu (txa (assoc cpu :x 0x44))]
            (should= (:a new-cpu) 0x44))))

      (describe "tay"
        (check-zero-flag-sets #(tay (assoc %1 :a 0)))
        (check-zero-flag-unsets #(tay (assoc %1 :a 1)))
        (check-negative-flag-sets #(tay (assoc %1 :a 0x80)))
        (check-negative-flag-unsets #(tay (assoc %1 :a 0)))

        (it "should transfer the value in the accumulator to the y register"
          (let [new-cpu (tay (assoc cpu :a 0x44))]
            (should= (:y new-cpu) 0x44))))

      (describe "tax"
        (check-zero-flag-sets #(tax (assoc %1 :a 0)))
        (check-zero-flag-unsets #(tax (assoc %1 :a 1)))
        (check-negative-flag-sets #(tax (assoc %1 :a 0x80)))
        (check-negative-flag-unsets #(tax (assoc %1 :a 0)))

        (it "should transfer the value in the accumulator to the x register"
          (let [new-cpu (tax (assoc cpu :a 0x44))]
            (should= (:x new-cpu) 0x44)))))

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

    (describe "logical operations"
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
