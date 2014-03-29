(ns clones.nes.memory
  (:require [clones.device      :refer :all]
            [clones.nes.mappers :refer :all]))

(declare mem-read)
(declare mem-write)

(defn- mem-read-internal-ram [bus addr]
  (let [mirrored-addr (bit-and 0x7ff addr)]
    [(get (:internal-ram bus) mirrored-addr 0) bus]))

(defn- mem-write-internal-ram [bus v addr]
  (let [mirrored-addr (bit-and 0x7ff addr)
        ram (:internal-ram bus)
        after-write (assoc ram mirrored-addr v)]
    [v (assoc bus :internal-ram after-write)]))

(defn- mem-read-device [bus device-name addr]
  (let [[v new-device] (device-read (device-name bus) addr)]
    [v (assoc bus device-name new-device)]))

(defn- mem-write-device [bus device-name v addr]
  (let [[_ new-device] (device-write (device-name bus) v addr)]
    [v (assoc bus device-name new-device)]))

(defn- mem-read-ppu [bus addr]
  (let [relative-addr (bit-and 7 addr)]
    (mem-read-device bus :ppu relative-addr)))

(defn- mem-write-ppu [bus v addr]
  (let [relative-addr (bit-and 7 addr)]
    (mem-write-device bus :ppu v relative-addr)))

(defn- mem-read-apu [bus addr]
  (let [relative-addr (bit-and 0x1f addr)]
    (mem-read-device bus :apu relative-addr)))

(defn- mem-write-apu [bus v addr]
  (let [relative-addr (bit-and 0x1f addr)]
    (mem-write-device bus :apu v relative-addr)))

(defn- mem-read-mapper [bus addr]
  (let [[v new-device] (prg-read (:mapper bus) addr)]
    [v (assoc bus :mapper new-device)]))

(defn- mem-write-mapper [bus v addr]
  (let [[_ new-device] (prg-write (:mapper bus) v addr)]
    [v (assoc bus :mapper new-device)]))

(defn- mem-write-dma-at [bus addr]
  (let [[v after-read] (mem-read bus addr)
        [_ after-write] (mem-write bus v 0x2004)]
    after-write))

(defn- mem-write-dma [bus v addr]
  (let [start-addr (bit-shift-left v 8)
        end-addr (+ 0x100 start-addr)
        addrs (range start-addr end-addr)
        after-dma (reduce mem-write-dma-at bus addrs)]
    [v after-dma]))

(defn mem-read [bus addr]
  (cond
    (< addr 0x2000) (mem-read-internal-ram bus addr)
    (< addr 0x4000) (mem-read-ppu bus addr)
    (< addr 0x4020) (mem-read-apu bus addr)
    :else           (mem-read-mapper bus addr)))

(defn mem-read-word [bus addr]
  (let [[high bus] (mem-read bus (+ 1 addr))
        [low bus] (mem-read bus addr)]
    [(bit-or (bit-shift-left high 8) low) bus]))

(defn mem-write [bus v addr]
  (cond
    (< addr 0x2000) (mem-write-internal-ram bus v addr)
    (< addr 0x4000) (mem-write-ppu bus v addr)
    (< addr 0x4014) (mem-write-apu bus v addr)
    (= addr 0x4014) (mem-write-dma bus v addr)
    (< addr 0x4020) (mem-write-apu bus v addr)
    :else           (mem-write-mapper bus v addr)))
