(ns clones.ppu
  (:require [clones.device :refer :all]
            [clones.byte   :refer :all]))

(defn control-write [ppu v]
  (let [vram-latch (bit-and (:vram-latch ppu) 0xf3ff)
        base-nametable (bit-and 3 v)]
    (merge ppu {:control                 v
                :vram-latch              (bit-or
                                           (bit-shift-left base-nametable 10)
                                           vram-latch)
                :base-nametable-addr     base-nametable
                :vram-addr-inc           (bit-and 1 (bit-shift-right v 2))
                :sprite-pattern-addr     (bit-and 1 (bit-shift-right v 3))
                :background-pattern-addr (bit-and 1 (bit-shift-right v 4))
                :sprite-size             (bit-and 1 (bit-shift-right v 5))
                :nmi-on-vblank?          (bit-set? v 7)})))

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
  (let [at-vblank-tick? (and
                          (= 1 (:tick ppu))
                          (= 240 (:scanline ppu)))
        vblank-started? (if at-vblank-tick?
                          false
                          (:vblank-started? ppu))
        status (-> 0
                 (bit-or (if vblank-started? 0x80 0))
                 (bit-or (if (:sprite-0-hit? ppu) 0x40 0))
                 (bit-or (if (:sprite-overflow? ppu) 0x20 0)))
        changes {:write-latch? true
                 :vblank-started? false
                 :suppress-vblank? at-vblank-tick?
                 :suppress-nmi? at-vblank-tick?}]
    [status (merge ppu changes)]))

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
    7 (data-read ppu)
    [0 ppu]))

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

(defn- rendering-enabled? [ppu]
  (or (:show-sprites? ppu) (:show-background? ppu)))

(defn- step-pre-render-scanline [machine]
  (let [ppu (:ppu machine)]
    (condp = (:tick ppu)
      1 (assoc machine :ppu
               (merge ppu {:sprite-0-hit? false
                           :sprite-overflow? false
                           :vblank-started? false}))
      304 (if (rendering-enabled? ppu)
            (assoc-in machine [:ppu :vram-addr] (:vram-latch ppu))
            machine)
      machine)))

(defn- step-post-render-scanline [machine]
  (let [ppu (:ppu machine)]
    (if (= 1 (:tick ppu))
      (let [request-nmi? (and
                           (:nmi-on-vblank? ppu)
                           (not (:suppress-nmi? ppu)))
            ppu-after-vblank (-> ppu
                               (assoc :vblank-started?
                                      (not (:suppress-vblank? ppu)))
                               (merge {:suppress-vblank? false
                                       :suppress-nmi? false}))
            nmi (if request-nmi? :nmi nil)]
        (merge machine {:ppu ppu-after-vblank :interrupt nmi}))
      machine)))

(defn- inc-coarse-y [ppu]
  (let [;; Get the vram addr with the fine y cleared to 0
        vram-addr (bit-and (:vram-addr ppu) 0xfff)
        old-coarse-y (bit-shift-right (bit-and vram-addr 0x03e0) 5)]
    (if (= 29 old-coarse-y)
      (let [new-vram-addr (-> vram-addr
                            (bit-and 0xfc1f)
                            (bit-xor 0x800))]
        (assoc ppu :vram-addr new-vram-addr))

      (let [new-coarse-y (bit-and 0x1f (inc old-coarse-y))
            new-vram-addr (-> vram-addr
                            (bit-and 0xfc1f)
                            (bit-or (bit-shift-left new-coarse-y 5)))]
        (assoc ppu :vram-addr new-vram-addr)))))

(defn- inc-fine-y [ppu]
  (let [vram-addr (:vram-addr ppu)
        fine-y-overflow? (= 0x7000 (bit-and vram-addr 0x7000))]
    (if fine-y-overflow?
      (inc-coarse-y ppu)
      (let [new-vram-addr (+ vram-addr 0x1000)]
        (assoc ppu :vram-addr new-vram-addr)))))

(defn- step-visible-scanline [machine]
  (let [ppu (:ppu machine)]
    (if (and (rendering-enabled? ppu) (= 256 (:tick ppu)))
      (assoc machine :ppu (inc-fine-y ppu))
      machine)))

(defn- advance-scanline [ppu]
  (let [scanline (:scanline ppu)
        tick (:tick ppu)]
    (if (= 340 tick)
      (merge ppu {:tick 0
                  :scanline (if (= 260 scanline)
                              -1
                              (inc scanline))})
      (assoc ppu :tick (inc tick)))))

(defn ppu-step [machine]
  (let [ppu (:ppu machine)
        scanline (:scanline ppu)
        tick (:tick ppu)
        machine (cond
                  (= -1 scanline) (step-pre-render-scanline machine)

                  (and
                    (> scanline -1)
                    (< scanline 240)) (step-visible-scanline machine)

                  (= 240 scanline) (step-post-render-scanline machine)
                  :else machine)
        after-advancing (advance-scanline (:ppu machine))]
    (assoc machine :ppu after-advancing)))

