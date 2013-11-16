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

(defn oam-addr-write [ppu v]
  (assoc ppu :oam-addr v))

(defn- advance-oam-addr [ppu]
  (let [orig (:oam-addr ppu)
        incd (mod (inc orig) 0x100)]
    (assoc ppu :oam-addr incd)))

(defn oam-data-write [ppu v]
  (let [after-write (assoc (:oam-ram ppu) (:oam-addr ppu) v)]
    (-> ppu
      (advance-oam-addr)
      (assoc :oam-ram after-write))))

(defn oam-data-read [ppu]
  [(get (:oam-ram ppu) (:oam-addr ppu) 0)
   ppu])

(defn status-read [ppu]
  [(-> 0
     (bit-or (if (:vblank-started? ppu) 0x80 0))
     (bit-or (if (:sprite-0-hit? ppu) 0x40 0))
     (bit-or (if (:sprite-overflow? ppu) 0x20 0)))
   (assoc ppu :write-latch? true)])

(defn register-write [ppu v addr]
  [v (condp = addr
       0 (control-write ppu v)
       1 (mask-write ppu v)
       3 (oam-addr-write ppu v)
       4 (oam-data-write ppu v))])

(defn register-read [ppu addr]
  (condp = addr
    2 (status-read ppu)
    4 (oam-data-read ppu)))

(defrecord PPU
  [^int control
   ^int base-nametable-address
   ^int vram-addr-inc
   ^int sprite-pattern-addr
   ^int background-pattern-addr
   ^int sprite-size
   ^boolean nmi-on-vblank?

   ^int mask
   ^boolean grayscale?
   ^boolean show-background-on-left?
   ^boolean show-sprites-on-left?
   ^boolean show-background?
   ^boolean show-sprites?
   ^boolean intense-reds?
   ^boolean intense-greens?
   ^boolean intense-blues?

   ^boolean sprite-overflow?
   ^boolean sprite-0-hit?
   ^boolean vblank-started?
   ^boolean write-latch?

   ^int oam-addr
   oam-ram]

  Device
  (device-read [this addr]
    (register-read this addr))

  (device-write [this v addr]
    (register-write this v addr)))

(def init-oam-ram (vec (repeat 0x100 0)))

(defn make-ppu []
  (PPU. 0 0 0 0 0 0 false
        0 false false false false false false false false
        false false false true
        0 init-oam-ram))
