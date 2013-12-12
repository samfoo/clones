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

(defn- nrom-read-chr-ram [nrom addr]
  (let [chr-data (:chr-data nrom)]
    [(get chr-data addr 0) nrom]))

(defn- nrom-write-chr-ram [nrom v addr]
  [v (assoc-in nrom [:chr-data addr] v)])

(defn- nrom-read-chr-rom [nrom addr]
  (let [^ints chr-data (:chr-data nrom)]
    (if (and (> 0xfff addr) (> (:chr-banks nrom) 1))
      [(aget chr-data addr) nrom]
      [(aget chr-data addr) nrom])))

(defrecord NROM [^int prg-banks
                 ^ints prg-rom

                 prg-ram

                 ^int chr-banks
                 ^ints chr-data
                 ^boolean chr-ram?

                 mirroring]
  Mapper
  (chr-read [this addr]
    (if chr-ram?
      (nrom-read-chr-ram this addr)
      (nrom-read-chr-rom this addr)))

  (chr-write [this v addr]
    (if chr-ram?
      (nrom-write-chr-ram this v addr)
      (throw (ex-info
               "Invalid memory access on NROM cartidge"
               {:addr addr :type :write}))))

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
  (let [chr-data (if (:chr-ram? rom)
                   {}
                   (int-array (:chr-data rom)))]
    (NROM. (:prg-banks rom) (int-array (:prg-data rom))
           {}
           (:chr-banks rom) chr-data
           (:chr-ram? rom)
           (:mirroring rom))))

