(ns clones.ppu.memory-spec
  (:require [speclj.core        :refer :all]
            [clones.device      :refer :all]
            [clones.nes.mappers :refer :all]
            [clones.ppu.memory  :refer :all]))

(defrecord MockMapper [prg chr]
  Mapper
  (prg-read [this addr] [(get prg addr 0) this])
  (prg-write [this v addr] [v (assoc this :prg (assoc prg addr v))])
  (chr-read [this addr] [(get chr addr 0) this])
  (chr-write [this v addr] [v (assoc this :chr (assoc chr addr v))]))

(defn stub-mapper [prg chr] (MockMapper. prg chr))
(def bus (make-ppu-memory {:mirroring :horizontal}))

(describe
  "The PPU's memory bus mapped with devices like the pattern tables, nametables, etc"
  (tags :memory)

  (describe "$3f00 0 $3fff"
    (it "should write to palette RAM relative to $3f00"
      (let [after-write (second (device-write bus 0xff 0x3f00))]
        (should= 0xff (first (device-read after-write 0x3f00)))))

    (it "should mirror writes at $3F10/$3F14/$3F18/$3F1C to $3F00/$3F04/$3F08/$3F0C"
      (let [w (fn [b v addr] (second (device-write b v addr)))
            bus-with-value (-> bus
                             (w 0xff 0x3f10)
                             (w 0xf4 0x3f14)
                             (w 0xf8 0x3f18)
                             (w 0xfc 0x3f1c))]
        (should= 0xf4 (first (device-read bus-with-value 0x3f04)))
        (should= 0xf8 (first (device-read bus-with-value 0x3f08)))
        (should= 0xfc (first (device-read bus-with-value 0x3f0c)))
        (should= 0xff (first (device-read bus-with-value 0x3f00)))))

    (it "should mirror reads at $3F10/$3F14/$3F18/$3F1C to $3F00/$3F04/$3F08/$3F0C"
      (let [bus-with-value (assoc bus
                                  :palette-ram
                                  {0   0xff
                                   4   0xf4
                                   8   0xf8
                                   0xc 0xfc})]
        (should= 0xf4 (first (device-read bus-with-value 0x3f14)))
        (should= 0xf8 (first (device-read bus-with-value 0x3f18)))
        (should= 0xfc (first (device-read bus-with-value 0x3f1c)))
        (should= 0xff (first (device-read bus-with-value 0x3f10)))))

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
      (let [bus-with-mapper (assoc bus
                                   :mapper
                                   (stub-mapper {} {}))
            after-write (second (device-write bus-with-mapper 0xff 0))]
        (should= 0xff (first (device-read after-write 0)))))

    (it "should read from the pattern tables"
      (let [bus-with-value (assoc bus
                                  :mapper
                                  (stub-mapper {} {0 0xff}))]
        (should= 0xff (first (device-read bus-with-value 0)))))))

