(ns clones.nes.memory
  (:require [clones.device      :refer :all]
            [clones.nes.mappers :refer :all]))

(defn- bus-read-internal-ram [bus addr]
  (let [mirrored-addr (bit-and 0x7ff addr)]
    [(get (:internal-ram bus) mirrored-addr 0) bus]))

(defn- bus-write-internal-ram [bus v addr]
  (let [mirrored-addr (bit-and 0x7ff addr)
        ram (:internal-ram bus)
        after-write (assoc ram mirrored-addr v)]
    [v (assoc bus :internal-ram after-write)]))

(defn- bus-read-device [bus device-name addr]
  (let [[v new-device] (device-read (device-name bus) addr)]
    [v (assoc bus device-name new-device)]))

(defn- bus-write-device [bus device-name v addr]
  (let [[_ new-device] (device-write (device-name bus) v addr)]
    [v (assoc bus device-name new-device)]))

(defn- bus-read-ppu [bus addr]
  (let [relative-addr (bit-and 7 addr)]
    (bus-read-device bus :ppu relative-addr)))

(defn- bus-write-ppu [bus v addr]
  (let [relative-addr (bit-and 7 addr)]
    (bus-write-device bus :ppu v relative-addr)))

(defn- bus-read-apu [bus addr]
  (let [relative-addr (bit-and 0x1f addr)]
    (bus-read-device bus :apu relative-addr)))

(defn- bus-write-apu [bus v addr]
  (let [relative-addr (bit-and 0x1f addr)]
    (bus-write-device bus :apu v relative-addr)))

(defn- bus-read-mapper [bus addr]
  (let [[v new-device] (prg-read (:mapper bus) addr)]
    [v (assoc bus :mapper new-device)]))

(defn- bus-write-mapper [bus v addr]
  (let [[_ new-device] (prg-write (:mapper bus) v addr)]
    [v (assoc bus :mapper new-device)]))

(defn- bus-write-dma-at [bus addr]
  (let [[v after-read] (device-read bus addr)
        [_ after-write] (device-write bus v 0x2004)]
    after-write))

(defn- bus-write-dma [bus v addr]
  (let [start-addr (bit-shift-left v 8)
        end-addr (+ 0x100 start-addr)
        addrs (range start-addr end-addr)
        after-dma (reduce bus-write-dma-at bus addrs)]
    [v after-dma]))

(defn- bus-read [bus addr]
  (cond
    (< addr 0x2000) (bus-read-internal-ram bus addr)
    (< addr 0x4000) (bus-read-ppu bus addr)
    (< addr 0x4020) (bus-read-apu bus addr)
    :else           (bus-read-mapper bus addr)))

(defn- bus-write [bus v addr]
  (cond
    (< addr 0x2000) (bus-write-internal-ram bus v addr)
    (< addr 0x4000) (bus-write-ppu bus v addr)
    (< addr 0x4014) (bus-write-apu bus v addr)
    (= addr 0x4014) (bus-write-dma bus v addr)
    (< addr 0x4020) (bus-write-apu bus v addr)
    :else           (bus-write-mapper bus v addr)))

(defrecord Bus [internal-ram
                ppu
                apu
                mapper]
  Device
  (device-read [this addr] (bus-read this addr))
  (device-write [this v addr] (bus-write this v addr)))

(defn make-main-memory [ppu apu mapper]
  (Bus. {} ppu apu mapper))
