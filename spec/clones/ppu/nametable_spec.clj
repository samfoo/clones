(ns clones.ppu.nametable-spec
  (:require [speclj.core          :refer :all]
            [clones.device        :refer :all]
            [clones.ppu.nametable :refer :all]))

(def horizontal (make-nametables :horizontal))
(def vertical (make-nametables :vertical))

(describe "PPU nametables"
  (defn check-mirroring [mirrored addr-tables]
    (describe "nametable I/O"
      (describe "device-write"
        (for [[addr table] addr-tables]
          (it (str "should write to nametable " table " when the address is "
                   "between $" (format "%04x" addr) " and $"
                   (format "%04x" (+ 0x400 addr -1)))
            (let [after-write (second (device-write mirrored 0xbe addr))]
              (should= 0xbe (get (table after-write) 0 0))))))

      (describe "device-read"
        (for [[addr table] addr-tables]
          (it (str "should read from nametable " table " when the address is "
                   "between $" (format "%04x" addr) " and $"
                   (format "%04x" (+ 0x400 addr -1)))
            (let [nametable-w-data (assoc mirrored table {0 0xbe})
                  v (first (device-read nametable-w-data addr))]
              (should= 0xbe v)))))))

  (describe "with vertical mirroring"
    (def addr-tables {0xc00 :nametable-1
                      0x800 :nametable-0
                      0x400 :nametable-1
                      0x000 :nametable-0})

    (check-mirroring vertical addr-tables))

  (describe "with horizontal mirroring"
    (def addr-tables {0xc00 :nametable-1
                      0x800 :nametable-1
                      0x400 :nametable-0
                      0x000 :nametable-0})
    (check-mirroring horizontal addr-tables)))

