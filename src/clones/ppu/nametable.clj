(ns clones.ppu.nametable
  (:require [clones.device :refer :all]))

(defn- nametable-with-horizontal-mirroring [logical-table]
  (condp = logical-table
    0 :nametable-0
    1 :nametable-0
    2 :nametable-1
    3 :nametable-1))

(defn- nametable-with-vertical-mirroring [logical-table]
  (condp = logical-table
    0 :nametable-0
    1 :nametable-1
    2 :nametable-0
    3 :nametable-1))

(defn- nametable-for-addr [nametables addr]
  (let [logical-table (bit-shift-right (bit-and 0xc00 addr) 10)]
    (condp = (:mirroring nametables)
      :horizontal (nametable-with-horizontal-mirroring logical-table)
      :vertical   (nametable-with-vertical-mirroring logical-table))))

(defn- nametable-read [nametables addr]
  (let [relative-addr (bit-and addr 0x3ff)
        nametable-n (nametable-for-addr nametables addr)]
    (get (nametable-n nametables) relative-addr 0)))

(defn- nametable-write [nametables v addr]
  (let [relative-addr (bit-and addr 0x3ff)
        nametable-n (nametable-for-addr nametables addr)
        after-write (assoc (nametable-n nametables) relative-addr v)]
    (assoc nametables nametable-n after-write)))

(defrecord Nametables [mirroring
                       nametable-0
                       nametable-1]
  Device
  (device-read [this addr] [(nametable-read this addr) this])
  (device-write [this v addr] [v (nametable-write this v addr)]))

(defn make-nametables [mirroring]
  (Nametables. mirroring {} {}))
