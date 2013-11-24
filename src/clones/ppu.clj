(ns clones.ppu
  (:require [clones.device :refer :all]
            [clones.byte   :refer :all]))

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

(defn- scroll-write-horizontal-offset [ppu v]
  (let [new-fine-x (bit-and 7 v)
        new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0x7fe0)
                         (bit-or (bit-shift-right v 3)))]
    (merge ppu {:fine-x new-fine-x
                :vram-latch new-vram-latch})))

(defn- scroll-write-vertical-offset [ppu v]
  (let [scanline (bit-and 7 v)
        distance-from-top (bit-shift-right v 3)
        new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0x0fff)
                         (bit-or (bit-shift-left scanline 12))
                         (bit-and 0xfc1f)
                         (bit-or (bit-shift-left distance-from-top 5)))]
    (assoc ppu :vram-latch new-vram-latch)))

(defn scroll-write [ppu v]
  (let [after-write (if (:write-latch? ppu)
                      (scroll-write-horizontal-offset ppu v)
                      (scroll-write-vertical-offset ppu v))]
    (assoc after-write :write-latch? (not (:write-latch? ppu)))))

(defn- addr-write-first [ppu v]
  (let [upper-6-bits (bit-and 0x3f v)
        new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0xff)
                         (bit-or (bit-shift-left upper-6-bits 8)))]
    (assoc ppu :vram-latch new-vram-latch)))

(defn- addr-write-second [ppu v]
  (let [new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0x3f00)
                         (bit-or v))]
    (merge ppu {:vram-latch new-vram-latch
                :vram-addr new-vram-latch})))

(defn addr-write [ppu v]
  (let [after-write (if (:write-latch? ppu)
                      (addr-write-first ppu v)
                      (addr-write-second ppu v))]
    (assoc after-write :write-latch? (not (:write-latch? ppu)))))

(defn- advance-vram-addr [ppu]
  (assoc ppu :vram-addr (if (= 0 (:vram-addr-inc ppu))
                          (+ 1 (:vram-addr ppu))
                          (+ 0x20 (:vram-addr ppu)))))

(defn data-write [ppu v]
  (let [memory (:memory ppu)
        memory-after-write (second (device-write memory v (:vram-addr ppu)))
        after-write (assoc ppu :memory memory-after-write)]
    (advance-vram-addr after-write)))

(defn- data-read-buffered [ppu]
  (let [result (:vram-data-buffer ppu)
        memory (:memory ppu)
        new-vram-data-buffer (first (device-read memory (:vram-addr ppu)))]
    [result (assoc ppu :vram-data-buffer new-vram-data-buffer)]))

(defn- data-read-unbuffered [ppu]
  (let [memory (:memory ppu)
        result (first (device-read memory (:vram-addr ppu)))
        buffer-fill-addr (- (:vram-addr ppu) 0x1000)
        new-vram-data-buffer (first (device-read memory buffer-fill-addr))]
    [result (assoc ppu :vram-data-buffer new-vram-data-buffer)]))

(defn data-read [ppu]
  (let [[result after-read] (if (< (:vram-addr ppu) 0x3f00)
                              (data-read-buffered ppu)
                              (data-read-unbuffered ppu))]
    [result (advance-vram-addr after-read)]))

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
       4 (oam-data-write ppu v)
       5 (scroll-write ppu v)
       6 (addr-write ppu v)
       7 (data-write ppu v))])

(defn register-read [ppu addr]
  (condp = addr
    2 (status-read ppu)
    4 (oam-data-read ppu)
    7 (data-read ppu)))

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
   oam-ram

   ^int fine-x
   ^int vram-latch
   ^int vram-addr
   ^int vram-data-buffer

   ^int scanline
   ^int tick

   memory]

  Device
  (device-read [this addr]
    (register-read this addr))

  (device-write [this v addr]
    (register-write this v addr)))

(def init-oam-ram (vec (repeat 0x100 0)))

(defn make-ppu [bus]
  (PPU. 0 0 0 0 0 0 false
        0 false false false false false false false false
        false false false true
        0 init-oam-ram
        0 0 0 0
        -1 0
        bus))

(defn- step-pre-render-scanline [ppu]
  (if (= 1 (:tick ppu))
    (merge ppu {:sprite-0-hit? false
                :sprite-overflow? false
                :vblank-started? false})
    ppu))

(defn- step-post-render-scanline [ppu]
  (if (= 1 (:tick ppu))
    (assoc ppu :vblank-started? true)
    ppu))

(defn- advance-ppu [ppu]
  (let [scanline (:scanline ppu)
        tick (:tick ppu)]
    (if (= 340 tick)
      (merge ppu {:tick 0
                  :scanline (if (= 260 scanline)
                              -1
                              (inc scanline))})
      (assoc ppu :tick (inc tick)))))

(defn ppu-step [ppu]
  (let [scanline (:scanline ppu)
        tick (:tick ppu)
        new-ppu (cond
                  (= -1 scanline) (step-pre-render-scanline ppu)
                  (= 240 scanline) (step-post-render-scanline ppu)
                  :else ppu)]
    (advance-ppu new-ppu)))

