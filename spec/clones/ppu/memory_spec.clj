(ns clones.ppu.memory-spec
  (:require [speclj.core       :refer :all]
            [clones.device     :refer :all]
            [clones.ppu.memory :refer :all]))

(def bus (make-memory {} {}))

(describe
  "The PPU's memory bus mapped with devices like the pattern tables, nametables, etc"

  (describe "$3f00 0 $3fff"
    (it "should write to palette RAM relative to $3f00"
      (let [after-write (second (device-write bus 0xff 0x3f00))]
        (should= 0xff (first (device-read after-write 0x3f00)))))

    (it "should mirror every 32 bytes when reading"
      (let [bus-with-value (assoc bus
                                  :palette-ram
                                  {0 0xff})]
        (should= 0xff (first (device-read bus-with-value 0x3f20)))
        (should= 0xff (first (device-read bus-with-value 0x3f40)))))

    (it "should read from palette RAM relative to $3f00"
      (let [bus-with-value (assoc bus
                                  :palette-ram
                                  {0 0xff})]
        (should= 0xff (first (device-read bus-with-value 0x3f00))))))

  (describe "$2000 - $3eff"
    (it "should write to the name tables relative to $2000"
      (let [after-write (second (device-write bus 0xff 0x2000))]
        (should= 0xff (first (device-read after-write 0x2000)))))

    (it "should mirror to $2xxx when reading from $3xxx"
      (let [bus-with-value (assoc bus
                                  :nametables
                                  {0 0xff})]
        (should= 0xff (first (device-read bus-with-value 0x3000)))))

    (it "should read from the name tables relative to $2000"
      (let [bus-with-value (assoc bus
                                  :nametables
                                  {0 0xff})]
        (should= 0xff (first (device-read bus-with-value 0x2000))))))

  (describe "$0000 - $1fff"
    (it "should write to the pattern tables"
      (let [after-write (second (device-write bus 0xff 0))]
        (should= 0xff (first (device-read after-write 0)))))

    (it "should read from the pattern tables"
      (let [bus-with-value (assoc bus
                                  :pattern-tables
                                  {0 0xff})]
        (should= 0xff (first (device-read bus-with-value 0)))))))

