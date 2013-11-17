(ns clones.cpu.memory-spec
  (:require [speclj.core         :refer :all]
            [clones.cpu.memory   :refer :all]
            [clojure.algo.monads :refer :all]))

(def mounts {})

(describe "Memory devices"
  (describe "reading & writing"
    (let [bus {0 0xff 1 0xee}
          xpu {:memory bus}]
      (describe "monadic I/O"
        (describe "io-write"
          (it "should return the mutated device as the second element"
            (let [[_ new-xpu] ((domonad state-m [a (io-write 1 0)] a) xpu)
                  new-bus (:memory new-xpu)
                  written (get new-bus 0 :should-be-1)]
              (should= 1 written)))

          (it "should return the written byte as the first element"
            (let [[result _] ((domonad state-m [a (io-write 1 0)] a) xpu)]
              (should= 1 result))))

        (describe "io-read-word"
          (it "should return the device as the second element"
            (let [[_ new-xpu] ((domonad state-m [a (io-read-word 0)] a) xpu)]
              (should= xpu new-xpu)))

          (it "should read two bytes starting at {addr} where the high byte is
               at {addr + 1} and the low byte is at {addr}, and return that
               word as the first element"
            (let [[result _] ((domonad state-m [a (io-read-word 0)] a) xpu)]
              (should= 0xeeff result))))

        (describe "io-read"
          (it "should return the device as the second element"
            (let [[_ new-xpu] ((domonad state-m [a (io-read 0)] a) xpu)]
              (should= xpu new-xpu)))

          (it "should read a single byte and return it as the first element"
            (let [[result _] ((domonad state-m [a (io-read 0)] a) xpu)]
              (should= 0xff result))))))))
