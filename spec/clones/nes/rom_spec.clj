(ns clones.nes.rom-spec
  (:require [speclj.core    :refer :all]
            [clones.nes.rom :refer :all]))

(def invalid-header (repeat 16 0))
(def valid-header [78 69 83 26 1 1 0 0 0 0 0 0 0 0 0 0])

(def with-mapper-1 [78 69 83 26 1 1 16 0 0 0 0 0 0 0 0 0])
(def with-mapper-16 [78 69 83 26 1 1 0 16 0 0 0 0 0 0 0 0])
(def with-mapper-17 [78 69 83 26 1 1 16 16 0 0 0 0 0 0 0 0])

(def with-four-screen-mirroring [78 69 83 26 1 1 8 0 0 0 0 0 0 0 0 0])
(def with-horizontal-mirroring [78 69 83 26 1 1 0 0 0 0 0 0 0 0 0 0])
(def with-vertical-mirroring [78 69 83 26 1 1 1 0 0 0 0 0 0 0 0 0])

(def with-trainer [78 69 83 26 1 1 4 0 0 0 0 0 0 0 0 0 0])
(def with-battery [78 69 83 26 1 1 2 0 0 0 0 0 0 0 0 0 0])

(def with-playchoice [78 69 83 26 1 1 0 2 0 0 0 0 0 0 0 0 0])
(def with-vs-unisystem [78 69 83 26 1 1 0 1 0 0 0 0 0 0 0 0 0])

(def with-nes-2 [78 69 83 26 1 1 0 8 0 0 0 0 0 0 0 0 0])

(describe "Making a mapper from a ROM"
  (describe "make-mapper"
    (it "should throw and exception if the mapper is unsupported"
      (should-throw clojure.lang.ExceptionInfo "Unsupported mapper"
        (make-mapper {:mapper -1})))))

(describe "An iNES ROM parser"
  (describe "parse-ines-header"
    (it "should read that it is not NES-2 format if bits 3 & 2 of the 7th byte aren't 10"
      (should-not (:nes-2? (parse-ines-header valid-header))))

    (it "should read that it is NES-2 format if bits 3 & 2 of the 7th byte are 10"
      (should (:nes-2? (parse-ines-header with-nes-2))))

    (it "should read that it is not a vs-unisystem if bit 0 of the 7th byte is 0"
      (should-not (:vs-unisystem? (parse-ines-header valid-header))))

    (it "should read that it is a vs-unisystem if bit 0 of the 7th byte is 1"
      (should (:vs-unisystem? (parse-ines-header with-vs-unisystem))))

    (it "should read that it is not a playchoice-10 if bit 1 of the 7th byte is 0"
      (should-not (:playchoice-10? (parse-ines-header valid-header))))

    (it "should read that it is a playchoice-10 if bit 1 of the 7th byte is 1"
      (should (:playchoice-10? (parse-ines-header with-playchoice))))

    (it "should read that there is no battery backing if bit 1 of the 6th byte is 0"
      (should-not (:battery-backed? (parse-ines-header valid-header))))

    (it "should read that there is a battery backing if bit 1 of the 6th byte is 1"
      (should (:battery-backed? (parse-ines-header with-battery))))

    (it "should read that there is no trainer if bit 2 of the 6th byte is 0"
      (should-not (:trainer-present? (parse-ines-header valid-header))))

    (it "should read that there is a trainer if bit 2 of the 6th byte is 1"
      (should (:trainer-present? (parse-ines-header with-trainer))))

    (it "should read the mirroring mode as the combination of the third and
        0th bit in the 6th byte

        0xx0 -> horizontal mirroring
        0xx1 -> vertical mirroring
        1xxx -> four-way mirroring"
      (should= :vertical (:mirroring (parse-ines-header with-vertical-mirroring)))
      (should= :horizontal (:mirroring (parse-ines-header with-horizontal-mirroring)))
      (should= :four-screen (:mirroring (parse-ines-header with-four-screen-mirroring))))

    (it "should or both the lower and upper nibbles for the mapper together"
      (should= 17
               (:mapper (parse-ines-header with-mapper-17))))

    (it "should read the mapper number upper nibble as the first four bits of
        the 7th byte"
      (should= 16
               (:mapper (parse-ines-header with-mapper-16))))

    (it "should read the mapper number lower nibble as the first four bits of
         the 6th byte"
      (should= 1
               (:mapper (parse-ines-header with-mapper-1))))

    (it "should read the 6th byte as the pattern table size in 8KB units converted to number of bytes"
      (should= 8192
               (:chr-rom-size (parse-ines-header valid-header))))

    (it "should read the 5th byte as the program rom size in 16KB units converted to number of bytes"
      (should= 16384
               (:prg-rom-size (parse-ines-header valid-header))))

    (it "should throw an error if the first four bytes don't match the expected signature"
      (should-throw Error "Invalid iNES file"
        (parse-ines-header invalid-header)))))
