(ns clones.ppu
  (:require [clones.cpu.memory :refer :all]))

(defn control-write [ppu v]
  (merge ppu {:control                 v
              :base-nametable-addr     (bit-and 3 v)
              :vram-addr-inc           (bit-and 1 (bit-shift-right v 2))
              :sprite-pattern-addr     (bit-and 1 (bit-shift-right v 3))
              :background-pattern-addr (bit-and 1 (bit-shift-right v 4))
              :sprite-size             (bit-and 1 (bit-shift-right v 5))
              :nmi-on-vblank?          (= 1 (bit-shift-right v 7))}))

(defn register-write [ppu v addr]
  (condp = addr
    0 (control-write ppu v)))

(defrecord PPU
  [control
   base-nametable-address
   vram-addr-inc
   sprite-pattern-addr
   background-pattern-addr
   sprite-size
   nmi-on-vblank?]

  Device
  (device-read [this addr] [nil nil])

  (device-write [this v addr]
    [v (register-write this v addr)]))

(defn make-ppu []
  (PPU. 0 0 0 0 0 0 false))
