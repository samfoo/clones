(ns clones.nes.mappers.nrom
  (:require [clones.cpu.memory :refer :all]))

(defn- nrom-read-ram [nrom addr]
  [(get (:prg-ram nrom) addr 0) nrom])

(defn- nrom-write-ram [nrom v addr]
  (let [ram (:prg-ram nrom)]
    [v (assoc nrom
              :prg-ram
              (assoc ram addr v))]))

(defn- nrom-read-prg-rom [nrom addr]
  (let [relative-addr (- addr 0x8000)
        ^ints prg-rom (:prg-rom nrom)]
    (if (and (> relative-addr 0x3fff) (> (:prg-banks nrom) 1))
      [(aget prg-rom relative-addr) nrom]
      [(aget prg-rom (bit-and 0x3fff relative-addr)) nrom])))

(defrecord NROM [^int prg-banks
                 prg-ram
                 ^ints prg-rom]
  Device
  (device-read [this addr]
    (cond
      (< addr 0x6000) (throw (ex-info
                               "Invalid memory access on NROM cartridge"
                               {:addr addr :type :read}))
      (< addr 0x8000) (nrom-read-ram this addr)
      :else (nrom-read-prg-rom this addr)))

  (device-write [this v addr]
    (cond
      (< addr 0x6000) (throw (ex-info
                               "Invalid memory access on NROM cartridge"
                               {:addr addr :type :write}))
      (< addr 0x8000) (nrom-write-ram this v addr)
      :else [v this])))

(defn nrom [rom]
  (NROM. (:prg-banks rom) {} (int-array (:prg-data rom))))

