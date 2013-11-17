(ns clones.ppu-specj
  (:require [speclj.core   :refer :all]
            [clones.device :refer :all]
            [clones.ppu    :refer :all]))

(def ppu (make-ppu {}))
(def ppu-latch-off (assoc ppu :write-latch? false))

(describe "The NES's 2C02 PPU"
  (describe "memory mapped register I/O"
    ;; NOTE: Remember that all devices are mounted to the memory bus, but that
    ;; device IO is relative. So the absolute read of $2000 would read $0000 on
    ;; the PPU

    (describe "make-ppu"
      (it "should have the write latch set initially"
        (should (:write-latch? (make-ppu {})))))

    (describe "read from the data register at $2007"
      (describe "when the address is >= $3f00"
        (it "should read directly from the PPU's memory bus"
          (let [ppu-w-data (merge ppu {:memory {0x3f00 0xff}
                                       :vram-addr 0x3f00})]
            (should= 0xff (first (device-read ppu-w-data 7)))))

        (it "should fill the vram data buffer with the value read from the bus
            $1000 below the current vram address"
          (let [ppu-w-data (merge ppu {:memory {0x2f00 0xff}
                                       :vram-addr 0x3f00})
                new-ppu (second (device-read ppu-w-data 7))]
            (should= 0xff (:vram-data-buffer new-ppu)))))

      (describe "when the address is < $3f00"
        (it "should read the current value of the vram data buffer"
          (let [result (first (device-read (assoc ppu :vram-data-buffer 0xee) 7))]
            (should= 0xee result)))

        (it "should fill the vram data buffer with the value read from the bus"
          (let [ppu-w-data (assoc ppu :memory {0 0xff})
                new-ppu (second (device-read ppu-w-data 7))]
            (should= 0xff (:vram-data-buffer new-ppu)))))

      (describe "when vram address increment is 1"
        (it "should increment the vram address by 32"
          (let [new-ppu (second (device-read (assoc ppu
                                                    :vram-addr-inc 1)
                                             7))]
            (should= 0x20 (:vram-addr new-ppu)))))

      (describe "when vram address increment is 0"
        (it "should increment the vram address by 1"
          (let [new-ppu (second (device-read (assoc ppu
                                                    :vram-addr-inc 0)
                                             7))]
            (should= 1 (:vram-addr new-ppu))))))

    (describe "write to the data register at $2007"
      (it "should write to the PPU's memory bus at the vram address"
        (let [new-ppu (second (device-write ppu 0xff 7))]
          (should= 0xff (first (device-read (:memory new-ppu) 0)))))

      (describe "when vram address increment is 1"
        (it "should increment the vram address by 32"
          (let [new-ppu (second (device-write (assoc ppu
                                                     :vram-addr-inc 1)
                                              0 7))]
            (should= 0x20 (:vram-addr new-ppu)))))

      (describe "when vram address increment is 0"
        (it "should increment the vram address by 1"
          (let [new-ppu (second (device-write (assoc ppu
                                                    :vram-addr-inc 0)
                                              0 7))]
            (should= 1 (:vram-addr new-ppu))))))

    (describe "write to the addr register at $2006"
      (describe "when the write latch is off"
        (it "should copy the vram latch into the vram address"
          (let [new-ppu (second (device-write ppu-latch-off 0xff 6))]
            (should= 0xff (:vram-addr new-ppu))))

        (it "should overwrite the lower 8 bits of the vram latch with the
            written value"
          (let [new-ppu (second (device-write (assoc ppu-latch-off
                                                     :vram-latch
                                                     0x3fff) 0 6))]
            (should= 0x3f00 (:vram-latch new-ppu))))

        (it "should update the lower 8 bits of the vram latch with the written
            value"
          (let [new-ppu (second (device-write ppu-latch-off 0xff 6))]
            (should= 0xff (:vram-latch new-ppu)))))

      (describe "when the write latch is on"
        (it "should clear bit 14 of the vram latch"
          (let [new-ppu (second (device-write (assoc ppu :vram-latch 0x7fff) 0xff 6))]
            (should= 0x3fff (:vram-latch new-ppu))))

        (it "should overwrite the existing bits 13-8 with the lower 6 bits of
            the 8 bit value that was written"
          (let [new-ppu (second (device-write (assoc ppu :vram-latch 0x3f00) 0 6))]
            (should= 0 (:vram-latch new-ppu))))

        (it "should update bits 13-8 with the lower 6 bits of the 8 bit value
            that was written"
          (let [new-ppu (second (device-write ppu 0x3f 6))]
            (should= 0x3f00 (:vram-latch new-ppu)))))

      (it "should flip the write latch"
        (let [off (second (device-write ppu 0 6))
              on (second (device-write off 0 6))]
          (should (:write-latch? on))
          (should-not (:write-latch? off)))))

    (describe "write to the scroll register at $2005"
      (describe "when the write latch is off (updating vertical offset)"
        (it "should overwrite the existing bit 9-5 of the 15 bit vram latch
            with the upper 5 bits of the 8 bit value that was written"
          (let [new-ppu (second (device-write (assoc
                                                ppu-latch-off
                                                :vram-latch 0x3e0) 0 5))]
            (should= 0 (:vram-latch new-ppu))))

        (it "should update bits 9-5 of the 15 bit vram latch with the upper 5
            bits of the 8 bit value that was written"
          (let [new-ppu (second (device-write ppu-latch-off 0xf8 5))]
            (should= 0x3e0 (:vram-latch new-ppu))))

        (it "should overwrite the existing upper 3 bits of the 15 bit vram latch
            with the lower 3 bits of the 8 bit value that was written"
          (let [new-ppu (second (device-write (assoc
                                                ppu-latch-off
                                                :vram-latch 0x7000) 0 5))]
            (should= 0 (:vram-latch new-ppu))))

        (it "should update the upper 3 bits of the 15 bit vram latch with the
            lower 3 bits of the 8 bit value that was written"
          (let [new-ppu (second (device-write ppu-latch-off 7 5))]
            (should= 0x7000 (:vram-latch new-ppu)))))

      (describe "when the write latch is on (updating horizontal offset)"
        (it "shouldn't alter the upper 10 bits of the 15 bit vram latch when
            updating the lower 5 bits"
          (let [new-ppu (second (device-write (assoc ppu :vram-latch 0x7fe0) 0xa8 5))]
            (should= 0x7ff5 (:vram-latch new-ppu))))

        (it "should overwrite the existing lower 5 bits of the 15 bit vram
            latch with the upper 5 bits of the 8 bit value that was written"
          (let [new-ppu (second (device-write (assoc ppu :vram-latch 0x15) 0 5))]
            (should= 0 (:vram-latch new-ppu))))

        (it "should update the lower 5 bits of the 15 bit vram latch with the
            upper 5 bits of the 8 bit value that was written"
          (let [new-ppu (second (device-write ppu 0xff 5))]
            (should= 0x1f (:vram-latch new-ppu))))

        (it "should copy the first three bits of the written value into the
            fine X internal register"
          (let [new-ppu (second (device-write ppu 0xff 5))]
            (should= 7 (:fine-x new-ppu)))))

      (it "should flip the write latch"
        (let [off (second (device-write ppu 0 5))
              on (second (device-write off 0 5))]
          (should (:write-latch? on))
          (should-not (:write-latch? off)))))

    (describe "read from the oam data register at $2004"
      (it "should read the value pointed at by the oam addr"
        (let [ppu-w-oam-data (assoc ppu :oam-ram {0 0xbe})]
          (should= 0xbe (first (device-read ppu-w-oam-data 4))))))

    (describe "write to the oam data register at $2004"
      (it "should modulo the oam address with 0x100 after incrementing"
        (let [ppu-w-oam-addr (assoc ppu :oam-addr 0xff)
              new-ppu (second (device-write ppu-w-oam-addr 0 4))]
          (should= 0 (:oam-addr new-ppu))))

      (it "should increment the oam address"
        (let [new-ppu (second (device-write ppu 0xff 4))]
          (should= 1 (:oam-addr new-ppu))))

      (it "should update the value pointed at by the oam addr"
        (let [ppu-w-oam-addr (assoc ppu :oam-addr 5)
              new-ppu (second (device-write ppu-w-oam-addr 0xff 4))]
          (should= 0xff (get (:oam-ram new-ppu) 5 0)))))

    (describe "write to the oam address register at $2003"
      (it "should set the oam address to the written value"
        (let [new-ppu (second (device-write ppu 0xff 3))]
          (should= 0xff (:oam-addr new-ppu)))))

    (describe "reading the status register at $2002"
      ;; TODO: Suppress NMI and suppress vblank states

      (it "should set the write latch to true"
        (let [ppu-w-latch-false (assoc ppu :write-latch? false)
              new-ppu (second (device-read ppu-w-latch-false 2))]
          (should (:write-latch? new-ppu))))

      (it "should have bit 7 unset if vblank hasn't started"
        (let [ppu-w-vbl (assoc ppu :vblank-started? false)]
          (should= 0 (first (device-read ppu-w-vbl 2)))))

      (it "should have bit 7 set if vblank has started"
        (let [ppu-w-vbl (assoc ppu :vblank-started? true)]
          (should= 0x80 (first (device-read ppu-w-vbl 2)))))

      (it "should have bit 6 unset if sprite 0 wasn't hit"
        (let [ppu-w-s0 (assoc ppu :sprite-0-hit? false)]
          (should= 0 (first (device-read ppu-w-s0 2)))))

      (it "should have bit 6 set if sprite 0 was hit"
        (let [ppu-w-s0 (assoc ppu :sprite-0-hit? true)]
          (should= 0x40 (first (device-read ppu-w-s0 2)))))

      (it "should have bit 5 unset if there wasn't a sprite overflow"
        (let [ppu-w-overflow (assoc ppu :sprite-overflow? false)]
          (should= 0 (first (device-read ppu-w-overflow 2)))))

      (it "should have bit 5 set if there was a sprite overflow"
        (let [ppu-w-overflow (assoc ppu :sprite-overflow? true)]
          (should= 0x20 (first (device-read ppu-w-overflow 2))))))

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
