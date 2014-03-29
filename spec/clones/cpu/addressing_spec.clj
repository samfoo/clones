(ns clones.cpu.addressing-spec
  (:require [speclj.core           :refer :all]
            [clojure.algo.monads   :refer :all]
            [clones.cpu            :refer :all]
            [clones.cpu.memory     :refer :all]
            [clones.cpu.addressing :refer :all]))

(def cpu (make-cpu))
(def cpu-with-zp
  (let [zp-addr 0x55
        [_ new-cpu] (io-> cpu
                          (io-write 0xbe zp-addr)
                          (io-write 0x55 (:pc cpu)))]
    new-cpu))

(defn do-mode-write [mode cpu v]
  (second (io-> cpu
                (mode-write mode v))))

(defn do-mode-read [mode cpu]
  (first (io-> cpu
               (mode-read mode))))

(defn do-mode [mode cpu]
  (first (io-> cpu (mode))))

(describe "6502 Operation Addressing Mode" (tags :addressing)
  (describe "mode-size"
    (it "should say all other modes are zero"
      (for [mode #{implied accumulator}]
        (should= 0 (mode-size mode))))

    (it "should say all modes that require one byte to read are one"
      (for [mode #{immediate zero-page zero-page-x zero-page-y indexed-indirect indirect-indexed relative}]
        (should= 1 (mode-size mode))))

    (it "should say all modes that require two bytes to read are two"
      (for [mode #{absolute absolute-x absolute-y indirect}]
        (should= 2 (mode-size mode)))))

  (describe "indirect-indexed"
    (it "should wrap the result if the address would be greater than two bytes"
      (let [[_ new-cpu] (io-> (merge cpu {:pc 1 :y 0xff})
                              (io-write 0xff 1)
                              (io-write 0xff 0)
                              (io-write 0xff 0xff))]
        (should= 0xfe (do-mode indirect-indexed new-cpu))))

    (it "should read the high byte from 0x00 when 'read(PC) + 1' would cross a page"
      (let [[_ new-cpu] (io-> (assoc cpu :pc 1)
                              (io-write 0xff 1)
                              (io-write 0xbe 0)
                              (io-write 0xef 0xff))]
        (should= 0xbeef (do-mode indirect-indexed new-cpu))))

    (it "should be 'readWord(read(PC)) + Y'"
      (let [[_ new-cpu] (io-> (merge cpu {:y 2 :pc 0})
                              ;; Target address: 0x05ff
                              ;; +------------------------+
                              ;; |addr: 00 | 01 | 02 | 03 |
                              ;; +------------------------+
                              ;; |val : 02 | 00 | fd | 05 |
                              ;; +------------------------+
                              ;;       ^          ^
                              ;;       PC         Pointer ref
                              (io-write 0x02 0)
                              (io-write 0xfd 2)
                              (io-write 0x05 3))]
        (should= 0x05ff (do-mode indirect-indexed new-cpu)))))

  (describe "indexed-indirect"
    (it "should read the high byte from 0x00 when 'read(PC) + X + 1' would cross a page"
      (let [[_ new-cpu] (io-> (assoc cpu :pc 1)
                              (io-write 0xff 1)
                              (io-write 0xbe 0)
                              (io-write 0xef 0xff))]
        (should= 0xbeef (do-mode indexed-indirect new-cpu))))

    (it "should be 'readWord(read(PC) + X)'"
      (let [[_ new-cpu] (io-> (merge cpu {:x 2 :pc 0})
                              ;; Target address: 0x1005
                              ;; +----------------------------------+
                              ;; |addr: 00 | 01 | 02 | 03 | 04 | 05 |
                              ;; +----------------------------------+
                              ;; |val : 02 | 00 | 00 | 00 | 05 | 10 |
                              ;; +----------------------------------+
                              ;;       ^                    ^
                              ;;       PC                   Pointer ref
                              (io-write 2 0)
                              (io-write 5 4)
                              (io-write 0x10 5))]
        (should= 0x1005 (do-mode indexed-indirect new-cpu)))))

  (describe "indirect"
    (it "should be the word at read($xx00) | read($xxff) if the address mode
        is indirect and the pointer points to the end of the page"
      (let [[_ new-cpu] (io-> cpu
                              (io-write-word 0x1ff 0)
                              (io-write 0xbe 0x100)
                              (io-write 0xef 0x1ff))]
        (should= 0xbeef (do-mode indirect new-cpu))))

    (it "should be 'readWord(readWord(PC))'"
      (let [[_ new-cpu] (io-> cpu
                              (io-write-word 0x100 0)
                              (io-write-word 0x200 0x100))]
        (should= 0x200 (do-mode indirect new-cpu)))))

  (describe "absolute-y"
    (it "should wrap if the address would be greater than 2 bytes"
      (let [[_ new-cpu] (io-> cpu
                              (io-write 0xff 0)
                              (io-write 0xff 1))]
        (should= 0xf (do-mode absolute-y (assoc new-cpu :y 0x10)))))

    (it "should use the absolute address and add the value of Y"
      (let [[_ new-cpu] (io-> cpu
                              (io-write 0xef 0)
                              (io-write 0xbe 1))]
        (should= 0xbeff (do-mode absolute-y (assoc new-cpu :y 0x10))))))

  (describe "absolute-x"
    (it "should wrap if the address would be greater than 2 bytes"
      (let [[_ new-cpu] (io-> cpu
                              (io-write 0xff 0)
                              (io-write 0xff 1))]
        (should= 0xf (do-mode absolute-x (assoc new-cpu :x 0x10)))))

    (it "should use the absolute address and add the value of X"
      (let [[_ new-cpu] (io-> cpu
                              (io-write 0xef 0)
                              (io-write 0xbe 1))]
        (should= 0xbeff (do-mode absolute-x (assoc new-cpu :x 0x10))))))

  (describe "absolute"
    (it "should be 'readWord(PC)'"
      (let [[_ new-cpu] (io-> cpu
                              (io-write 0xef 0)
                              (io-write 0xbe 1))]
        (should= 0xbeef (do-mode absolute new-cpu)))))

  (describe "relative"
    (it "should wrap if the result would be < 0"
      (let [[_ new-cpu] (io-> cpu
                              (io-write 0x80 (:pc cpu)))]
        (should= 0xff81 (do-mode relative new-cpu))))

    ;; (it "should wrap if the result would be > 0xffff"
    ;;   (let [cpu-with-pc (merge cpu {:pc 0xffff
    ;;                                 :mapper {}})
    ;;         [_ new-cpu] (io-> cpu-with-pc
    ;;                           (io-write 0x79 (:pc cpu-with-pc)))]
    ;;     (should= 0x79 (do-mode relative new-cpu))))

    (it "should be 'read(PC) + (PC - 0x100) + 1' if read(PC) is >= 0x80"
      (let [cpu-with-pc (assoc cpu :pc 0x1000)
            [_ new-cpu] (io-> cpu-with-pc
                              (io-write 0x80 (:pc cpu-with-pc)))]
        (should= 0x0f81 (do-mode relative new-cpu))))

    (it "should be 'read(PC) + PC + 1' if read(PC) is < 0x80"
      (let [cpu-with-pc (assoc cpu :pc 0x1000)
            [_ new-cpu] (io-> cpu-with-pc
                              (io-write 0x79 (:pc cpu-with-pc)))]
        (should= 0x107a (do-mode relative new-cpu)))))

  (describe "zero-page-y"
    (it "should wrap the resulting address to the first page if it would cross a page"
      (let [cpu-with-zp-y (assoc cpu-with-zp :y 0xff)]
        (should= 0x0054 (do-mode zero-page-y cpu-with-zp-y))))

    (it "should use the zero-page address, and add the contents of the Y register"
      (let [cpu-with-zp-y (assoc cpu-with-zp :y 0x10)]
        (should= 0x0065 (do-mode zero-page-y cpu-with-zp-y)))))

  (describe "zero-page-x"
    (it "should wrap the resulting address to the first page if it would cross a page"
      (let [cpu-with-zp-x (assoc cpu-with-zp :x 0xff)]
        (should= 0x0054 (do-mode zero-page-x cpu-with-zp-x))))

    (it "should use the zero-page address, and add the contents of the X register"
      (let [cpu-with-zp-x (assoc cpu-with-zp :x 0x10)]
        (should= 0x0065 (do-mode zero-page-x cpu-with-zp-x)))))

  (describe "zero-page"
    (it "should be '0x0000 + read(PC)'"
      (should= 0x0055 (do-mode zero-page cpu-with-zp))))

  (describe "immediate"
    (it "should use whatever value PC points at"
      (let [new-cpu (assoc cpu :pc 0xbeef)]
        (should= 0xbeef (do-mode immediate new-cpu)))))

  (describe "implied"
    (it "should raise an error trying to read or write from the impled address mode"
      (should-throw Error "Can't read/write to the implied address mode"
        (do-mode-write implied cpu 0xbe))

      (should-throw Error "Can't read/write to the implied address mode"
        (do-mode-read implied cpu))))

  (describe "accumulator"
    (it "should write to the accumulator"
        (let [cpu-with-acc (do-mode-write accumulator cpu 0xbe)]
          (should= 0xbe (:a cpu-with-acc))))

    (it "should read from the accumulator"
      (let [cpu-with-acc (assoc cpu :a 0xbe)
            result (do-mode-read accumulator cpu-with-acc)]
        (should= 0xbe result)))))
