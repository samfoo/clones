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

    (describe "writing to the mask register at $2001"
      (defn check-mask [m field]
        (let [on (second (device-write ppu m 1))
              off (second (device-write (assoc ppu field true) 0 1))]
          (should (field on))
          (should-not (field off))))

      (it "should set the mask property to the written value"
        (let [new-ppu (second (device-write ppu 0xbe 1))]
          (should= 0xbe (:mask new-ppu))))

      (it "should set intense blues based on the value at bit 7"
        (check-mask 0x80 :intense-blues?))

      (it "should set intense greens based on the value at bit 6"
        (check-mask 0x40 :intense-greens?))

      (it "should set intense reds based on the value at bit 5"
        (check-mask 0x20 :intense-reds?))

      (it "should set showing sprites based on the value at bit 4"
        (check-mask 0x10 :show-sprites?))

      (it "should set showing the background based on the value at bit 3"
        (check-mask 8 :show-background?))

      (it "should set showing sprites on the left-most 8 pixels of the screen
          based on the value at bit 2"
        (check-mask 4 :show-sprites-on-left?))

      (it "should set showing the background on the left-most 8 pixels of the
          screen based on the value at bit 1"
        (check-mask 2 :show-background-on-left?))

      (it "should turn on/off grayscale display based on the value at bit 0"
        (check-mask 1 :grayscale?)))

    (describe "writing to the control register at $2000"
      (it "should turn on/off NMI on vertical blank based on the value at bit 7"
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
              horizontal (second (device-write ppu 0 0))]
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
