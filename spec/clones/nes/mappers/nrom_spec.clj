(ns clones.nes.mappers.nrom-spec
  (:require [speclj.core             :refer :all]
            [clones.device           :refer :all]
            [clones.nes.mappers      :refer :all]
            [clones.nes.mappers.nrom :refer :all]))

(def rom-defaults {:mirroring :horizontal
                   :prg-banks 1
                   :prg-data (vec (repeat 0x8000 0))
                   :chr-banks 1
                   :chr-ram? false
                   :chr-data (vec (repeat 0x2000 0))})

(def rom (nrom rom-defaults))

(describe "The NROM mapper" (tags :nrom)
  (describe "graphics ROM"
    (describe "$0000 - $0fff"
      (describe "when there is chr RAM instead of ROM"
        (it "should read from chr RAM"
          (let [nrom-w-ram (nrom (merge rom-defaults {:chr-ram? true
                                                      :chr-banks 0}))
                nrom-w-ram-val (assoc-in nrom-w-ram [:chr-data 0x100] 0xbe)]
            (should= 0xbe (first (chr-read nrom-w-ram-val 0x100)))))

        (it "should write to chr RAM"
          (let [nrom-w-ram (nrom (merge rom-defaults {:chr-ram? true
                                                      :chr-banks 0}))
                [_ after-write] (chr-write nrom-w-ram 0xbe 0)]
            (should= 0xbe (first (chr-read after-write 0))))))

      (let [single-bank (assoc (vec (repeat 0x2000 0)) 0 0xbe)
            rom-w-1-bank (nrom (merge rom-defaults {:chr-banks 1
                                                    :chr-data single-bank}))]
        (it "should read from the first chr bank"
          (should= 0xbe (first (chr-read rom-w-1-bank 0)))))))

  (describe "program ROM"
    (describe "$c000 - $ffff"
      (it "should do nothing on writing"
        (let [after-write (second (prg-write rom 0xff 0xc000))]
          (should= 0 (first (prg-read after-write 0xc000)))))

      (let [single-bank (assoc (vec (repeat 0x8000 0)) 0 0xbe)
            rom-w-1-bank (nrom (merge rom-defaults {:prg-banks 1
                                                   :prg-data single-bank}))]
        (describe "when there's only one bank of program ROM"
          (it "should read from the first bank of program ROM mirrored down"
            (should= 0xbe (first (prg-read rom-w-1-bank 0xc000))))))

      (let [two-banks (assoc (vec (repeat 0x8000 0)) 0x4000 0xbe)
            rom-w-2-banks (nrom (merge rom-defaults {:prg-banks 2
                                                     :prg-data two-banks}))]
        (describe "when there's two banks of program ROM"
          (it "should read from the second bank of program ROM"
            (should= 0xbe (first (prg-read rom-w-2-banks 0xc000)))))))

    (describe "$8000 - $bfff"
      (it "should do nothing on writing"
        (let [after-write (second (prg-write rom 0xff 0x8000))]
          (should= 0 (first (prg-read after-write 0x8000)))))

      (let [single-bank (assoc (vec (repeat 0x8000 0)) 0 0xbe)
            rom-w-1-bank (nrom (merge rom-defaults {:prg-banks 1
                                                    :prg-data single-bank}))]
        (it "should read from the first bank of program ROM relative to $8000"
          (should= 0xbe (first (prg-read rom-w-1-bank 0x8000))))))

    (describe "$6000 - $7fff"
      (it "should write to program RAM"
        (let [after-write (second (prg-write rom 0xff 0x6000))]
          (should= 0xff (first (prg-read after-write 0x6000)))))

      (it "should read from program RAM"
        (let [rom-w-ram (assoc rom :prg-ram {0x6000 0xbe})]
          (should= 0xbe (first (prg-read rom-w-ram 0x6000))))))))

    ;; (describe "$4020 - $5fff (or anything less than $6000)"
    ;;   (it "should throw an exception on writing"
    ;;     (should-throw clojure.lang.ExceptionInfo "Invalid memory access on NROM cartridge"
    ;;       (prg-write rom 1 0x4020)))
    ;;
    ;;   (it "should throw an exception on reading"
    ;;     (should-throw clojure.lang.ExceptionInfo "Invalid memory access on NROM cartridge"
    ;;       (prg-read rom 0x4020))))))
    ;;
