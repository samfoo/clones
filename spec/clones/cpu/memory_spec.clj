(ns clones.cpu.memory-spec
  (:require [speclj.core         :refer :all]
            [clones.cpu.memory   :refer :all]
            [clojure.algo.monads :refer :all]))

(def mounts {})
(def bus (make-memory-bus {} {} {}))

(describe "The CPU's memory bus"
  (defn should-return-bus-after-reading [addr]
    (it "should return the bus after reading"
      (should= bus (second (device-read bus addr)))))

  (describe "$4020 - $ffff"
    (should-return-bus-after-reading 0x6000)

    (it "should write to the mapper"
      (let [after-write (second (device-write bus 0xff 0x4020))]
        (should= 0xff (first (device-read after-write 0x4020)))))

    (it "should read from the mapper"
      (let [bus-with-value (assoc bus
                                  :mapper
                                  {0x6000 0xbe})]
        (should= 0xbe (first (device-read bus-with-value 0x6000))))))

  (describe "$4000 - $40ff"
    (should-return-bus-after-reading 0x4000)

    (it "should write to the APU"
      (let [after-write (second (device-write bus 0xff 0x4000))]
        (should= 0xff (first (device-read after-write 0x4000)))))

    (it "should read from the APU relative to $4000"
      (let [bus-with-value (assoc bus
                                  :apu
                                  {0 0xbe})]
        (should= 0xbe (first (device-read bus-with-value 0x4000))))))

  (describe "$2000 - $3fff"
    (should-return-bus-after-reading 0x2000)

    (it "should mirror all addresses > $2007 to $2000 - $2007 when writing"
      (let [after-write (second (device-write bus 0xff 0x2008))]
        (should= 0xff (first (device-read after-write 0x2000)))))

    (it "should write to the PPU"
      (let [after-write (second (device-write bus 0xff 0x2000))]
        (should= 0xff (first (device-read after-write 0x2000)))))

    (it "should mirror all addresses > $2007 to $2000 - $2007 when reading"
      (let [bus-with-value (assoc bus
                                  :ppu
                                  {0 0xbe})]
        (should= 0xbe (first (device-read bus-with-value 0x2008)))))

    (it "should read from the PPU relative to $2000"
      (let [bus-with-value (assoc bus
                                  :ppu
                                  {0 0xbe})]
        (should= 0xbe (first (device-read bus-with-value 0x2000))))))

  (describe "$0000 - $1fff"
    (should-return-bus-after-reading 0)

    (it "should mirror all addresses > $07ff to $0000 - $07ff when writing"
      (let [after-write (second (device-write bus 0xff 0x800))]
        (should= 0xff (first (device-read after-write 0)))))

    (it "should write to internal RAM"
      (let [after-write (second (device-write bus 0xff 0))]
        (should= 0xff (first (device-read after-write 0)))))

    (it "should mirror all addresses > $07ff to $0000 - $07ff when reading"
      (let [bus-with-value (assoc bus
                                  :internal-ram
                                  {0 0xbe})]
        (should= 0xbe (first (device-read bus-with-value 0x800)))))

    (it "should read from internal RAM"
      (let [bus-with-value (assoc bus
                                  :internal-ram
                                  {0 0xbe})]
        (should= 0xbe (first (device-read bus-with-value 0)))))))

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
