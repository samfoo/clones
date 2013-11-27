(ns clones.nes.mappers.nrom
  (:require [clones.device      :refer :all]
            [clones.nes.mappers :refer :all]))

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

(defn- nrom-read-chr-rom [nrom addr]
  (let [^ints chr-rom (:chr-rom nrom)]
    (if (and (> 0xfff addr) (> (:chr-banks nrom) 1))
      [(aget chr-rom addr) nrom]
      [(aget chr-rom (bit-and 0xfff addr)) nrom])))

(defrecord NROM [^int prg-banks
                 ^ints prg-rom

                 prg-ram

                 ^int chr-banks
                 ^ints chr-rom

                 mirroring]
  Mapper
  (chr-read [this addr]
    (nrom-read-chr-rom this addr))

  (chr-write [this v addr]
    (throw (ex-info
             "Invalid memory access on NROM cartidge"
             {:addr addr :type :write})))

  (prg-read [this addr]
    (cond
      (< addr 0x6000) (throw (ex-info
                               "Invalid memory access on NROM cartridge"
                               {:addr addr :type :read}))
      (< addr 0x8000) (nrom-read-ram this addr)
      :else (nrom-read-prg-rom this addr)))

  (prg-write [this v addr]
    (cond
      (< addr 0x6000) (throw (ex-info
                               "Invalid memory access on NROM cartridge"
                               {:addr addr :type :write}))
      (< addr 0x8000) (nrom-write-ram this v addr)
      :else [v this])))

(defn nrom [rom]
  (NROM. (:prg-banks rom) (int-array (:prg-data rom))
         {}
         (:chr-banks rom) (int-array (:chr-data rom))
         (:mirroring rom)))

