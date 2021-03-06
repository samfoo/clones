(ns clones.ppu
  (:require [clones.device :refer :all]
            [clones.byte   :refer :all])
  (import [java.awt.image BufferedImage]))

(defn control-write [ppu v]
  (let [vram-latch (bit-and (:vram-latch ppu) 0xf3ff)
        base-nametable (bit-and 3 v)]
    (-> ppu
      (assoc! :control                 v)
      (assoc! :vram-latch              (bit-or
                                         (bit-shift-left base-nametable 10)
                                         vram-latch))
      (assoc! :base-nametable-addr     base-nametable)
      (assoc! :vram-addr-inc           (bit-and 1 (bit-shift-right v 2)))
      (assoc! :sprite-pattern-addr     (bit-and 1 (bit-shift-right v 3)))
      (assoc! :background-pattern-addr (bit-and 1 (bit-shift-right v 4)))
      (assoc! :sprite-size             (bit-and 1 (bit-shift-right v 5)))
      (assoc! :nmi-on-vblank?          (bit-set? v 7)))))

(defn mask-write [ppu v]
  (-> ppu
    (assoc! :mask                     v)
    (assoc! :grayscale?               (bit-set? v 0))
    (assoc! :show-background-on-left? (bit-set? v 1))
    (assoc! :show-sprites-on-left?    (bit-set? v 2))
    (assoc! :show-background?         (bit-set? v 3))
    (assoc! :show-sprites?            (bit-set? v 4))
    (assoc! :intense-reds?            (bit-set? v 5))
    (assoc! :intense-greens?          (bit-set? v 6))
    (assoc! :intense-blues?           (bit-set? v 7))))

(defn oam-addr-write [ppu v]
  (assoc! ppu :oam-addr v))

(defn- advance-oam-addr [ppu]
  (let [orig (:oam-addr ppu)
        incd (mod (inc orig) 0x100)]
    (assoc! ppu :oam-addr incd)))

(defn oam-data-write [ppu v]
  (let [after-write (assoc (:oam-ram ppu) (:oam-addr ppu) v)]
    (-> ppu
      (advance-oam-addr)
      (assoc! :oam-ram after-write))))

(defn- scroll-write-horizontal-offset [ppu v]
  (let [new-fine-x (bit-and 7 v)
        new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0x7fe0)
                         (bit-or (bit-shift-right v 3)))]
    (-> ppu
      (assoc! :fine-x new-fine-x)
      (assoc! :vram-latch new-vram-latch))))

(defn- scroll-write-vertical-offset [ppu v]
  (let [scanline (bit-and 7 v)
        distance-from-top (bit-shift-right v 3)
        new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0x0fff)
                         (bit-or (bit-shift-left scanline 12))
                         (bit-and 0xfc1f)
                         (bit-or (bit-shift-left distance-from-top 5)))]
    (assoc! ppu :vram-latch new-vram-latch)))

(defn scroll-write [ppu v]
  (let [after-write (if (:write-latch? ppu)
                      (scroll-write-horizontal-offset ppu v)
                      (scroll-write-vertical-offset ppu v))]
    (assoc! after-write :write-latch? (not (:write-latch? ppu)))))

(defn- addr-write-first [ppu v]
  (let [upper-6-bits (bit-and 0x3f v)
        new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0xff)
                         (bit-or (bit-shift-left upper-6-bits 8)))]
    (assoc! ppu :vram-latch new-vram-latch)))

(defn- addr-write-second [ppu v]
  (let [new-vram-latch (-> (:vram-latch ppu)
                         (bit-and 0x3f00)
                         (bit-or v))]
    (-> ppu
      (assoc! :vram-latch new-vram-latch)
      (assoc! :vram-addr new-vram-latch))))

(defn addr-write [ppu v]
  (let [after-write (if (:write-latch? ppu)
                      (addr-write-first ppu v)
                      (addr-write-second ppu v))]
    (assoc! after-write :write-latch? (not (:write-latch? ppu)))))

(defn- advance-vram-addr [ppu]
  (assoc! ppu :vram-addr (if (zero? (:vram-addr-inc ppu))
                          (inc (:vram-addr ppu))
                          (+ 0x20 (:vram-addr ppu)))))

(defn data-write [ppu v]
  (let [memory (:memory ppu)
        memory-after-write (second (device-write memory v (:vram-addr ppu)))
        after-write (assoc! ppu :memory memory-after-write)]
    (advance-vram-addr after-write)))

(defn- data-read-buffered [ppu]
  (let [result (:vram-data-buffer ppu)
        memory (:memory ppu)
        new-vram-data-buffer (first (device-read memory (:vram-addr ppu)))]
    [result (assoc! ppu :vram-data-buffer new-vram-data-buffer)]))

(defn- data-read-unbuffered [ppu]
  (let [memory (:memory ppu)
        result (first (device-read memory (:vram-addr ppu)))
        buffer-fill-addr (- (:vram-addr ppu) 0x1000)
        new-vram-data-buffer (first (device-read memory buffer-fill-addr))]
    [result (assoc! ppu :vram-data-buffer new-vram-data-buffer)]))

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
                 (bit-or (if (:sprite-overflow? ppu) 0x20 0)))]
    [status (-> ppu
              (assoc! :write-latch? true)
              (assoc! :vblank-started? false)
              (assoc! :suppress-vblank? at-vblank-tick?)
              (assoc! :suppress-nmi? at-vblank-tick?))]))

(defn ppu-write [ppu v addr]
  [v (condp = addr
       0 (control-write ppu v)
       1 (mask-write ppu v)
       3 (oam-addr-write ppu v)
       4 (oam-data-write ppu v)
       5 (scroll-write ppu v)
       6 (addr-write ppu v)
       7 (data-write ppu v)
       ppu)])

(defn ppu-register-write [machine v addr]
  (let [ppu (:ppu machine)]
    (do
      (ppu-write ppu v addr)
      [v machine])))

(defn ppu-read [ppu addr]
  (condp = addr
    2 (status-read ppu)
    4 (oam-data-read ppu)
    7 (data-read ppu)
    [0 ppu]))

(defn ppu-register-read [machine addr]
  (let [ppu (:ppu machine)
        [v new-ppu] (ppu-read ppu addr)]
    [v machine]))

(def init-oam-ram (vec (repeat 0x100 0)))

(defn make-ppu [bus]
  {:control 0
   :base-nametable-address 0
   :vram-addr-inc 0
   :sprite-pattern-addr 0
   :background-pattern-addr 0
   :sprite-size 0
   :nmi-on-vblank? false

   :mask 0
   :grayscale? false
   :show-background-on-left? false
   :show-sprites-on-left? false
   :show-background? false
   :show-sprites? false
   :intense-reds? false
   :intense-greens? false
   :intense-blues? false

   :sprite-overflow? false
   :sprite-0-hit? false
   :vblank-started? false
   :write-latch? true

   :oam-addr 0
   :oam-ram init-oam-ram

   :fine-x 0
   :vram-latch 0
   :vram-addr 0
   :vram-data-buffer 0

   :scanline 261
   :tick 0
   :frame-count 0

   :background-frame-buffer (BufferedImage. 256 240 BufferedImage/TYPE_INT_ARGB)
   :memory bus})

(defn- rendering-enabled? [ppu]
  (or (:show-sprites? ppu) (:show-background? ppu)))

(defn- step-pre-render-scanline [machine]
  (let [ppu (:ppu machine)]
    (condp = (:tick ppu)
      1 (do
          (-> ppu
            (assoc! :sprite-0-hit? false)
            (assoc! :sprite-overflow? false))
          machine)

      304 (do
            (when (rendering-enabled? ppu) (assoc! ppu :vram-addr (:vram-latch ppu)))
            machine)
      machine)))

(defn- step-post-render-scanline [machine]
  (let [ppu (:ppu machine)]
    (if (= 1 (:tick ppu))
      (let [request-nmi? (and
                           (:nmi-on-vblank? ppu)
                           (not (:suppress-nmi? ppu)))
            frame-count (if (rendering-enabled? ppu)
                          (inc (:frame-count ppu))
                          (:frame-count ppu))
            ppu-after-vblank (-> ppu
                               (assoc! :vblank-started? (not (:suppress-vblank? ppu)))
                               (assoc! :frame-count frame-count)
                               (assoc! :suppress-vblank? false)
                               (assoc! :suppress-nmi? false))
            nmi (when request-nmi? :nmi)]
        (assoc machine :interrupt nmi))
      machine)))

(defn- inc-coarse-y [ppu]
  (let [;; Get the vram addr with the fine y cleared to 0
        vram-addr (bit-and (:vram-addr ppu) 0xfff)
        old-coarse-y (bit-shift-right (bit-and vram-addr 0x03e0) 5)]
    (if (= 29 old-coarse-y)
      (let [new-vram-addr (-> vram-addr
                            (bit-and 0xfc1f)
                            (bit-xor 0x800))]
        (assoc! ppu :vram-addr new-vram-addr))

      (let [new-coarse-y (bit-and 0x1f (inc old-coarse-y))
            new-vram-addr (-> vram-addr
                            (bit-and 0xfc1f)
                            (bit-or (bit-shift-left new-coarse-y 5)))]
        (assoc! ppu :vram-addr new-vram-addr)))))

(defn- inc-fine-y [ppu]
  (let [vram-addr (:vram-addr ppu)
        fine-y-overflow? (= 0x7000 (bit-and vram-addr 0x7000))]
    (if fine-y-overflow?
      (inc-coarse-y ppu)
      (let [new-vram-addr (+ vram-addr 0x1000)]
        (assoc! ppu :vram-addr new-vram-addr)))))

(defn- inc-coarse-x [ppu]
  (let [vram-addr (:vram-addr ppu)
        old-coarse-x (bit-and vram-addr 0x1f)
        new-vram-addr (if (= 31 old-coarse-x)
                        (-> old-coarse-x
                          (bit-and 0xffe0)
                          (bit-xor 0x400))
                        (inc old-coarse-x))]
    (assoc! ppu :vram-addr new-vram-addr)))

(defn pattern-tile-row [ppu tile-index fine-y]
  "Read a row of palette indicies (0, 1, 2 or 3) from the pattern table.

  tile-index is the reference in the nametable."

  (let [pattern-table-addr (* 0x1000 (:background-pattern-addr ppu))
        start (-> tile-index
                (bit-shift-left 4)
                (bit-and 0x0ff0)
                (bit-or pattern-table-addr)
                (bit-or fine-y))
        memory (:memory ppu)
        pixel-row-low (first (device-read memory start))
        pixel-row-high (first (device-read memory (+ start 8)))]
    (reduce (fn [palette-indices i]
              (let [low-bit (-> pixel-row-low
                              (bit-shift-right i)
                              (bit-and 1))
                    high-bit (-> pixel-row-high
                               (bit-shift-right i)
                               (bit-and 1)
                               (bit-shift-left 1))
                    palette-index (bit-or low-bit high-bit)]
                (cons palette-index palette-indices)))
            []
            (range 8))))

(defn- vram-addr-for-scanline-tile [start-of-line-vram-addr tile-x-index]
  (let [vram-addr start-of-line-vram-addr
        coarse-x (bit-and vram-addr 0x1f)
        new-coarse-x (+ coarse-x tile-x-index)
        overflow? (> new-coarse-x 0x1f)
        result (-> vram-addr
                 (bit-and 0xffe0)
                 (bit-xor (if overflow? 0x400 0))
                 (bit-or (bit-and 0x1f new-coarse-x)))]
    result))

(defn pattern-tile-indices-for-current-scanline [ppu]
  ;; TODO: Fine-x scroll... I'm not entirely sure how it works.
  (let [memory (:memory ppu)
        vram-addr (:vram-addr ppu)]
    (map (fn [i]
           (let [nametable-addr (bit-or
                                  0x2000
                                  (bit-and
                                    0xfff
                                    (vram-addr-for-scanline-tile vram-addr i)))
                 tile-index (first (device-read memory nametable-addr))]
             tile-index))
         (range 32))))

(defn- get-color [pixel]
  (condp = pixel
    0 0
    1 0xffff0000
    2 0xff00ff00
    3 0xff0000ff))

(defn- render-background-for-current-scanline [ppu] ppu)
  ;; (let [scanline (:scanline ppu)
  ;;       frame-buffer (:background-frame-buffer ppu)
  ;;       fine-y (bit-shift-right (:vram-addr ppu) 12)
  ;;       tile-indices (pattern-tile-indices-for-current-scanline ppu)
  ;;       scanline-pattern (vec (flatten
  ;;                               (map #(pattern-tile-row ppu % fine-y)
  ;;                                    tile-indices)))]
  ;;   (doseq [x (range 256)]
  ;;     (let [color-index (nth scanline-pattern x)]
  ;;       (.setRGB frame-buffer x scanline (get-color color-index)))))
  ;; ppu)

(defn- maybe-render-background [ppu]
  (if (:show-background? ppu)
    (render-background-for-current-scanline ppu)
    ppu))

(defn- maybe-inc-fine-y [ppu]
  (if (rendering-enabled? ppu)
    (inc-fine-y ppu)
    ppu))

(defn- step-visible-scanline [machine]
  (let [ppu (:ppu machine)]
    (if (== 256 (:tick ppu))
      (do
        (-> ppu
          (maybe-render-background)
          (maybe-inc-fine-y))
        machine)
      machine)))

(defn- advance-odd-scanline [ppu]
  (-> ppu
    (assoc! :scanline 0)
    (assoc! :tick 0)))

(defn- advance-normal-scanline [ppu scanline tick]
  (if (== 340 tick)
    (-> ppu
      (assoc! :tick 0)
      (assoc! :scanline (mod (+ 1 scanline) 262)))
    (assoc! ppu :tick (+ 1 tick))))

(defn- advance-scanline [ppu]
  (let [^int scanline (:scanline ppu)
        ^int tick (:tick ppu)
        frame-count ^int (:frame-count ppu)]
    (if (and
          (== 339 tick)
          (== 261 scanline)
          (odd? frame-count)
          (:show-background? ppu))
      (advance-odd-scanline ppu)
      (advance-normal-scanline ppu scanline tick))))

(defn transient-ppu-step [machine]
  (let [ppu (:ppu machine)
        scanline (int (:scanline ppu))
        tick (int (:tick ppu))

        machine (cond
                  (< scanline 240) (step-visible-scanline machine)

                  (== 240 scanline) (step-post-render-scanline machine)

                  (and
                    (== 260 scanline)
                    (== 1 tick)) (do
                                   (assoc! ppu :vblank-started? false)
                                   machine)

                  (== 261 scanline) (step-pre-render-scanline machine)

                  :else machine)]
    (do
      (advance-scanline ppu)
      machine)))

(defn- transient-machine [nes]
  (let [ppu (:ppu nes)]
    (assoc nes :ppu (transient ppu))))

(defn- persistent-machine! [nes]
  (let [ppu (:ppu nes)]
    (assoc nes :ppu (persistent! ppu))))

(defn ppu-step [machine]
  (-> machine
    (transient-machine)
    (transient-ppu-step)
    (persistent-machine!)))
