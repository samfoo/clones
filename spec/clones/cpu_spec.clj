(ns clones.cpu-spec
  (:require [speclj.core :refer :all]
            [clones.cpu  :refer :all]))

(describe "The NES 6502 CPU"
  (let [cpu (make-cpu)]
    (describe "adc"
      (it "should add the argument to the accumulator"
        (let [new-cpu (adc cpu 0x01)]
          (should= (:a new-cpu) 0x01)))

      (it "should overflow if the addition would exceed 0xff"
          (let [new-cpu (adc (assoc cpu :a 0xff) 0x1)]
            (should= (:a new-cpu) 0x00)))

      (it "should set the zero flag when the result is zero"
        (let [new-cpu (adc cpu 0x00)]
          (should (zero-flag? new-cpu))))

      (it "should unset the zero flag when the result is non-zero"
        (let [
              cpu-with-zero (update-flags (make-cpu) zero-flag)
              new-cpu (adc cpu-with-zero 0x01)]
          (should-not (zero-flag? new-cpu))))

      (it "should set the negative flag when the result is negative"
        (let [new-cpu (adc cpu 0x80)]
          (should (negative-flag? new-cpu))))))

      (it "should unset the negative flag when the result is not negative"
        (let [
              cpu-with-negative (update-flags (make-cpu) negative-flag)
              new-cpu (adc cpu-with-negative 0x01)]
          (should-not (negative-flag? new-cpu))))

      (it "should set the overflow flag when adding two positives yields a negative"
        (let [cpu (merge (make-cpu) {:a 0x79})
              new-cpu (adc cpu 0x79)]
          (should (overflow-flag? new-cpu))))

      (it "should set the overflow flag when adding two negatives yields a positive"
        (let [cpu (merge (make-cpu) {:a 0x80})
              new-cpu (adc cpu 0x80)]
          (should (overflow-flag? new-cpu))))

      (let [cpu (update-flags (make-cpu) carry-flag)]
        (context "when the carry flag already set"
          (it "should add an additional 1 to the result"
            (let [new-cpu (adc cpu 0x00)]
              (should= (:a new-cpu) 0x01)))

          (it "should unset the carry flag"
            (let [new-cpu (adc cpu 0x01)]
              (should-not (carry-flag? new-cpu)))))))
