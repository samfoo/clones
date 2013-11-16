(ns clones.ppu-specj
  (:require [speclj.core       :refer :all]
            [clones.cpu.memory :refer :all]
            [clones.ppu        :refer :all]))

(def ppu (make-ppu))

(describe "The NES's 2C02 PPU"
  (describe "writing to the memory mapped registers"
    ;; NOTE: Remember that all devices are mounted to the memory bus, but that
    ;; device IO is relative. So the absolute read of $2000 would read $0000 on
    ;; the PPU

    (describe "PPUCTRL ($2000) > write"
      (it "should turn on/off NMI on vertical blank to the value at bit 7"
        (let [on (second (device-write ppu 0x80 0))
              off (second (device-write ppu 0 0))]
          (should (:nmi-on-vblank? on))
          (should-not (:nmi-on-vblank? off))))

      (it "should set the sprite size to the value at bit 5"
        (let [sz-8x8 (second (device-write ppu 0 0))
              sz-8x16 (second (device-write ppu 0x20 0))]
          (should= 1 (:sprite-size sz-8x16))
          (should= 0 (:sprite-size sz-8x8))))

      (it "should set the background pattern table address to the value at bit 4"
        (let [at-0000 (second (device-write ppu 0 0))
              at-1000 (second (device-write ppu 0x10 0))]
          (should= 1 (:background-pattern-addr at-1000))
          (should= 0 (:background-pattern-addr at-0000))))

      (it "should set the sprite pattern table address for 8x8 sprites to the
          value at bit 3"
        (let [at-0000 (second (device-write ppu 0 0))
              at-1000 (second (device-write ppu 8 0))]
          (should= 1 (:sprite-pattern-addr at-1000))
          (should= 0 (:sprite-pattern-addr at-0000))))

      (it "should set the VRAM address increment per CPU read/write of PPUDATA
          to the value at bit 2"
        (let [vertical (second (device-write ppu 4 0))
              horizontal (second (device-write (assoc ppu :vram-addr-inc 1) 0 0))]
          (should= 1 (:vram-addr-inc vertical))
          (should= 0 (:vram-addr-inc horizontal))))

      (it "should set the base nametable address to the first two bits"
        (let [at-2c00 (second (device-write ppu 3 0))
              at-2800 (second (device-write ppu 2 0))
              at-2400 (second (device-write ppu 1 0))
              at-2000 (second (device-write ppu 0 0))]
          (should= 0 (:base-nametable-addr at-2000))
          (should= 1 (:base-nametable-addr at-2400))
          (should= 2 (:base-nametable-addr at-2800))
          (should= 3 (:base-nametable-addr at-2c00))))

      (it "should set the control property to the written value"
        (let [new-ppu (second (device-write ppu 3 0))]
          (should= 3 (:control new-ppu)))))))
