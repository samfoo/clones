(ns clones.ppu.memory
  (:require [clones.device        :refer :all]
            [clones.nes.mappers   :refer :all]
            [clones.ppu.nametable :refer :all]))

(defn- bus-read-device [bus device-name addr]
  (let [[v new-device] (device-read (device-name bus) addr)]
    [v (assoc bus device-name new-device)]))

(defn- bus-write-device [bus device-name v addr]
  (let [[_ new-device] (device-write (device-name bus) v addr)]
    [v (assoc bus device-name new-device)]))

(defn- bus-read-pattern-tables [bus addr]
  (let [[v new-device] (chr-read (:mapper bus) addr)]
    [v (assoc bus :mapper new-device)]))

(defn- bus-write-pattern-tables [bus v addr]
  (let [[_ new-device] (chr-write (:mapper bus) v addr)]
    [v (assoc bus :mapper new-device)]))

(defn- bus-read-nametables [bus addr]
  (let [relative-addr (bit-and 0xfff addr)]
    (bus-read-device bus :nametables relative-addr)))

(defn- bus-write-nametables [bus v addr]
  (let [relative-addr (bit-and 0xfff addr)]
    (bus-write-device bus :nametables v relative-addr)))

(defn- bus-read-palette-ram [bus addr]
  (let [relative-addr (bit-and 0x1f addr)]
    (bus-read-device bus :palette-ram relative-addr)))

(defn- bus-write-palette-ram [bus v addr]
  (let [relative-addr (bit-and 0x1f addr)]
    (bus-write-device bus :palette-ram v relative-addr)))

(defn- bus-read [bus addr]
  (cond
    (< addr 0x2000) (bus-read-pattern-tables bus addr)
    (< addr 0x3eff) (bus-read-nametables bus addr)
    :else           (bus-read-palette-ram bus addr)))

(defn- bus-write [bus v addr]
  (cond
    (< addr 0x2000) (bus-write-pattern-tables bus v addr)
    (< addr 0x3eff) (bus-write-nametables bus v addr)
    :else           (bus-write-palette-ram bus v addr)))

(defrecord Bus [mapper
                nametables
                palette-ram]
  Device
  (device-read [this addr] (bus-read this addr))
  (device-write [this v addr] (bus-write this v addr)))

(defn make-ppu-memory [mapper]
  (Bus. mapper (make-nametables (:mirroring mapper)) {}))
