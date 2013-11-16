(ns clones.ppu
  (:require [clones.cpu.memory :refer :all]
            [clones.byte       :refer :all]))

(defn control-write [ppu v]
  (merge ppu {:control                 v
              :base-nametable-addr     (bit-and 3 v)
              :vram-addr-inc           (bit-and 1 (bit-shift-right v 2))
              :sprite-pattern-addr     (bit-and 1 (bit-shift-right v 3))
              :background-pattern-addr (bit-and 1 (bit-shift-right v 4))
              :sprite-size             (bit-and 1 (bit-shift-right v 5))
              :nmi-on-vblank?          (bit-set? v 7)}))

(defn mask-write [ppu v]
  (merge ppu {:mask                     v
              :grayscale?               (bit-set? v 0)
              :show-background-on-left? (bit-set? v 1)
              :show-sprites-on-left?    (bit-set? v 2)
              :show-background?         (bit-set? v 3)
              :show-sprites?            (bit-set? v 4)
              :intense-reds?            (bit-set? v 5)
              :intense-greens?          (bit-set? v 6)
              :intense-blues?           (bit-set? v 7)}))

(defn register-write [ppu v addr]
  (condp = addr
    0 (control-write ppu v)
    1 (mask-write ppu v)))

(defrecord PPU
  [control
   base-nametable-address
   vram-addr-inc
   sprite-pattern-addr
   background-pattern-addr
   sprite-size
   nmi-on-vblank?

   mask
   grayscale?
   show-background-on-left?
   show-sprites-on-left?
   show-background?
   show-sprites?
   intense-reds?
   intense-greens?
   intense-blues?]

  Device
  (device-read [this addr] [nil nil])

  (device-write [this v addr]
    [v (register-write this v addr)]))

(defn make-ppu []
  (PPU. 0 0 0 0 0 0 false
        0 false false false false false false false false))
