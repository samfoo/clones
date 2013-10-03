(ns clones.byte-spec
  (:require [speclj.core :refer :all]
            [clones.byte :refer :all]))

(describe "Unsigned bits, bytes and words"
  (describe "low-byte"
    (it "should return the lower byte of a 2 byte word"
      (should= 0xee (low-byte 0xffee))))

  (describe "high-byte"
    (it "should return the upper byte of a 2 byte word"
      (should= 0xff (high-byte 0xff00)))))
