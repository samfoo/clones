(ns clones.addressing-spec
  (:require [speclj.core       :refer :all]
            [clones.cpu        :refer :all]
            [clones.addressing :refer :all]))

(def cpu (make-cpu))
(def cpu-with-zp
  (let [zero-page-addr 0x55
        new-cpu (cpu-write cpu 0xbe zero-page-addr)]
    (cpu-write new-cpu 0x55 (:pc new-cpu))))

(describe "6502 Operation Addressing Mode"
  (describe "indirect-indexed"
    (it "should be 'readWord(read(PC)) + Y'"
      (let [new-cpu (-> cpu
                      ;; Target address: 0x05ff
                      ;; +------------------------+
                      ;; |addr: 00 | 01 | 02 | 03 |
                      ;; +------------------------+
                      ;; |val : 02 | 00 | fd | 05 |
                      ;; +------------------------+
                      ;;       ^          ^
                      ;;       PC         Pointer ref
                      (assoc :y 2)
                      (assoc :pc 0)
                      (cpu-write 0x02 0)
                      (cpu-write 0xfd 2)
                      (cpu-write 0x05 3))]
        (should= 0x05ff (mode-addr indirect-indexed new-cpu)))))

  (describe "indexed-indirect"
    (it "should be 'readWord(read(PC) + X)'"
      (let [new-cpu (-> cpu
                      ;; Target address: 0x1005
                      ;; +----------------------------------+
                      ;; |addr: 00 | 01 | 02 | 03 | 04 | 05 |
                      ;; +----------------------------------+
                      ;; |val : 02 | 00 | 00 | 00 | 05 | 10 |
                      ;; +----------------------------------+
                      ;;       ^                    ^
                      ;;       PC                   Pointer ref
                      (assoc :x 2)
                      (assoc :pc 0)
                      (cpu-write 0x02 0)
                      (cpu-write 0x05 4)
                      (cpu-write 0x10 5))]
        (should= 0x1005 (mode-addr indexed-indirect new-cpu)))))

  (describe "indirect"
    (it "should wrap the least significant byte of the indirect address if
        adding 1 to it would have wrapped to a new page"
      (let [new-cpu (-> cpu
                      (cpu-write 0xff 0)
                      (cpu-write 0x01 1)
                      (cpu-write 0x00 0x0100)
                      (cpu-write 0x02 (+ 1 0x0100)))]
        (should= 0x0200 (mode-addr indirect new-cpu))))

    (it "should be 'readWord(readWord(PC) + 1)'"
      (let [new-cpu (-> cpu
                      (cpu-write 0x00 0)
                      (cpu-write 0x01 1)
                      (cpu-write 0x00 (+ 1 0x0100))
                      (cpu-write 0x02 (+ 2 0x0100)))]
        (should= 0x0200 (mode-addr indirect new-cpu)))))

  (describe "absolute-y"
    (it "should use the absolute address and add the value of Y"
      (let [new-cpu (-> cpu
                      (cpu-write 0xef 0)
                      (cpu-write 0xbe 1)
                      (assoc :y 0x10))]
        (should= 0xbeff (mode-addr absolute-y new-cpu)))))

  (describe "absolute-x"
    (it "should use the absolute address and add the value of X"
      (let [new-cpu (-> cpu
                      (cpu-write 0xef 0)
                      (cpu-write 0xbe 1)
                      (assoc :x 0x10))]
        (should= 0xbeff (mode-addr absolute-x new-cpu)))))

  (describe "absolute"
    (it "should be 'readWord(PC)'"
      (let [new-cpu (-> cpu
                      (cpu-write 0xef 0)
                      (cpu-write 0xbe 1))]
        (should= 0xbeef (mode-addr absolute new-cpu)))))

  (describe "relative"
    (it "should be 'read(PC) + (PC - 0x100) + 1' if read(PC) is >= 0x80"
      (let [cpu-with-pc (assoc cpu :pc 0x1000)
            new-cpu (cpu-write cpu-with-pc 0x80 (:pc cpu-with-pc))]
        (should= 0x0f81 (mode-addr relative new-cpu))))

    (it "should be 'read(PC) + PC + 1' if read(PC) is < 0x80"
      (let [cpu-with-pc (assoc cpu :pc 0x1000)
            new-cpu (cpu-write cpu-with-pc 0x79 (:pc cpu-with-pc))]
        (should= 0x107a (mode-addr relative new-cpu)))))

  (describe "zero-page-y"
    (it "should wrap the resulting address to the first page if it would cross a page"
      (let [cpu-with-zp-y (assoc cpu-with-zp :y 0xff)]
        (should= 0x0054 (mode-addr zero-page-y cpu-with-zp-y))))

    (it "should use the zero-page address, and add the contents of the Y register"
      (let [cpu-with-zp-y (assoc cpu-with-zp :y 0x10)]
        (should= 0x0065 (mode-addr zero-page-y cpu-with-zp-y)))))

  (describe "zero-page-x"
    (it "should wrap the resulting address to the first page if it would cross a page"
      (let [cpu-with-zp-x (assoc cpu-with-zp :x 0xff)]
        (should= 0x0054 (mode-addr zero-page-x cpu-with-zp-x))))

    (it "should use the zero-page address, and add the contents of the X register"
      (let [cpu-with-zp-x (assoc cpu-with-zp :x 0x10)]
        (should= 0x0065 (mode-addr zero-page-x cpu-with-zp-x)))))

  (describe "zero-page"
    (it "should be '0x0000 + read(PC)'"
      (should= 0x0055 (mode-addr zero-page cpu-with-zp))))

  (describe "immediate"
    (it "should use whatever value PC points at"
      (let [new-cpu (assoc cpu :pc 0xbeef)]
        (should= 0xbeef (mode-addr immediate new-cpu)))))

  (describe "implied"
    (it "should raise an error trying to read or write from the impled address mode"
      (should-throw Error "Can't read/write to the implied address mode"
        (mode-write implied cpu 0xbe))
      (should-throw Error "Can't read/write to the implied address mode"
        (mode-read implied cpu))))

  (describe "accumulator"
    (it "should write to the accumulator"
      (let [cpu-with-acc (mode-write accumulator cpu 0xbe)]
        (should= 0xbe (:a cpu-with-acc))))

    (it "should read from the accumulator"
      (let [cpu-with-acc (assoc cpu :a 0xbe)]
        (should= 0xbe (mode-read accumulator cpu-with-acc))))))
