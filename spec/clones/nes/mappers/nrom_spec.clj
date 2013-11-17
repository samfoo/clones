(ns clones.nes.mappers.nrom-spec
  (:require [speclj.core             :refer :all]
            [clones.device           :refer :all]
            [clones.nes.mappers.nrom :refer :all]))

(def rom (nrom {:prg-banks 1
                :prg-data (vec (repeat 0x8000 0))}))

(describe "The NROM mapper"
  (describe "$c000 - $ffff"
    (it "should do nothing on writing"
      (let [after-write (second (device-write rom 0xff 0xc000))]
        (should= 0 (first (device-read after-write 0xc000)))))

    (let [single-bank (assoc (vec (repeat 0x8000 0)) 0 0xbe)
          rom-w-1-bank (nrom {:prg-banks 1
                              :prg-data single-bank})]
      (describe "when there's only one bank of program ROM"
        (it "should read from the first bank of program ROM mirrored down"
          (should= 0xbe (first (device-read rom-w-1-bank 0xc000))))))

    (let [two-banks (assoc (vec (repeat 0x8000 0)) 0x4000 0xbe)
          rom-w-2-banks (nrom {:prg-banks 2
                               :prg-data two-banks})]
      (describe "when there's two banks of program ROM"
        (it "should read from the second bank of program ROM"
          (should= 0xbe (first (device-read rom-w-2-banks 0xc000)))))))

  (describe "$8000 - $bfff"
    (it "should do nothing on writing"
      (let [after-write (second (device-write rom 0xff 0x8000))]
        (should= 0 (first (device-read after-write 0x8000)))))

    (let [single-bank (assoc (vec (repeat 0x8000 0)) 0 0xbe)
          rom-w-1-bank (nrom {:prg-banks 1
                              :prg-data single-bank})]
      (it "should read from the first bank of program ROM relative to $8000"
        (should= 0xbe (first (device-read rom-w-1-bank 0x8000))))))

  (describe "$6000 - $7fff"
    (it "should write to program RAM"
      (let [after-write (second (device-write rom 0xff 0x6000))]
        (should= 0xff (first (device-read after-write 0x6000)))))

    (it "should read from program RAM"
      (let [rom-w-ram (assoc rom :prg-ram {0x6000 0xbe})]
        (should= 0xbe (first (device-read rom-w-ram 0x6000))))))

  (describe "$4020 - $5fff (or anything less than $6000)"
    (it "should throw an exception on writing"
      (should-throw clojure.lang.ExceptionInfo "Invalid memory access on NROM cartridge"
        (device-write rom 1 0x4020)))

    (it "should throw an exception on reading"
      (should-throw clojure.lang.ExceptionInfo "Invalid memory access on NROM cartridge"
        (device-read rom 0x4020)))))

