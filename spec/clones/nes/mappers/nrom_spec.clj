(ns clones.nes.mappers.nrom-spec
  (:require [speclj.core             :refer :all]
            [clones.nes.mappers      :refer :all]
            [clones.nes.mappers.nrom :refer :all]))

(describe "The NROM mapper"
  (describe "program data"
    (describe "mapper-write-prg"
      (it "should not write anything"
        (let [rom (nrom {:prg-data {}
                         :prg-banks 1})]
          (should= rom (second (mapper-write-prg rom 0xff 1))))))

    (describe "mapper-read-prg"
      (it "should read from the second bank when the address is >= 0x4000 and
          there is more than one bank"
        (let [rom (nrom {:prg-data (assoc (vec (repeat 0x8000 0)) 0x4001 0xbe)
                         :prg-banks 2})]
          (should= 0xbe (first (mapper-read-prg rom 0x4001)))))

      (it "should read from the first bank, but with the address wrapped, when
          the address is >= 0x4000 and there's only one bank"
        (let [rom (nrom {:prg-data (assoc (vec (repeat 0x8000 0)) 1 0xbe)
                         :prg-banks 1})]
          (should= 0xbe (first (mapper-read-prg rom 0x4001)))))

      (it "should read from the first bank when the address is < 0x4000"
        (let [rom (nrom {:prg-data (assoc (vec (repeat 0x8000 0)) 1 0xbe)
                         :prg-banks 1})]
          (should= 0xbe (first (mapper-read-prg rom 1))))))))
