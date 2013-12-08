(ns clones.ppu.debug
  (:require [clones.device :refer :all]))

(defn- pattern-table-tile-row-at [ppu pattern-table-addr i row]
  (let [start (-> i
                (bit-shift-left 4)
                (bit-and 0x0ff0)
                (bit-or pattern-table-addr)
                (bit-or row))
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

(defn- pattern-table-tile-at [ppu pattern-table-addr i]
  (map
    (fn [row]
      (pattern-table-tile-row-at ppu pattern-table-addr i row))
    (range 8)))

(defn pattern-table-tiles [ppu table]
  (let [pattern-table-addr (condp = table
                             :left  0x0000
                             :right 0x1000)]
    (map
      (fn [i]
        (pattern-table-tile-at ppu pattern-table-addr i))
      (range 256))))
