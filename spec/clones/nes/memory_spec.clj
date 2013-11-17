(ns clones.nes.memory-spec
  (:require [speclj.core         :refer :all]
            [clones.nes.memory   :refer :all]
            [clones.device       :refer :all]
            [clojure.algo.monads :refer :all]))

(def bus (make-memory {} {} {}))

(describe
  "The NES's memory bus, mapped with devices like APU, PPU, and cartridge"

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

