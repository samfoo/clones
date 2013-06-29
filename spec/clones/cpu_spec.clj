(ns clones.cpu-spec
  (:require [speclj.core :refer :all]
            [clones.cpu  :refer :all]))

(declare ^:dynamic cpu)
(describe "The NES CPU"
  (describe "The instruction set"
    (around [it]
       (binding [cpu (make-cpu)]
         (it)))

    (describe "sbc"
      (it "should set the carry flag when the result is an unsigned underflow"
        (let [new-cpu (sbc (assoc cpu :a 0) 1)]
          (should (carry-flag? new-cpu))))

      (it "should unset the carry flag when the result is not an unsigned underflow"
        (let [cpu-with-carry (update-flags (assoc cpu :a 1) carry-flag)
              new-cpu (sbc cpu-with-carry 0)]
          (should-not (carry-flag? new-cpu))))

      (it "should set the zero flag when the result is zero"
        (let [new-cpu (sbc (assoc cpu :a 1) 0)]
          (should (zero-flag? new-cpu))))

      (it "should unset the zero flag when the result is non-zero"
        (let [
              cpu-with-zero (update-flags cpu zero-flag)
              new-cpu (sbc cpu-with-zero 1)]
          (should-not (zero-flag? new-cpu))))

      (it "should set the negative flag when the result is negative"
        (let [new-cpu (sbc cpu 0)]
          (should (negative-flag? new-cpu))))

      (it "should unset the negative flag when the result is not negative"
        (let [
              cpu-with-negative (update-flags (assoc cpu :a 1) negative-flag)
              new-cpu (sbc cpu-with-negative 0)]
          (should-not (negative-flag? new-cpu))))

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

      (it "should set the zero flag when the result is zero"
        (let [new-cpu (adc cpu 0)]
          (should (zero-flag? new-cpu))))

      (it "should unset the zero flag when the result is non-zero"
        (let [
              cpu-with-zero (update-flags cpu zero-flag)
              new-cpu (adc cpu-with-zero 1)]
          (should-not (zero-flag? new-cpu))))

      (it "should set the negative flag when the result is negative"
        (let [new-cpu (adc cpu 0x80)]
          (should (negative-flag? new-cpu))))

      (it "should unset the negative flag when the result is not negative"
        (let [
              cpu-with-negative (update-flags cpu negative-flag)
              new-cpu (adc cpu-with-negative 1)]
          (should-not (negative-flag? new-cpu))))

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
