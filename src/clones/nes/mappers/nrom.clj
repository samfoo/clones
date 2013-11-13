(ns clones.nes.mappers.nrom
  (:require [clones.nes.mappers :refer :all]))

(defn- mirrored-addr? [nrom addr]
  (and
    (> addr 0x3fff)
    (> (:prg-banks (.data nrom)) 1)))

(deftype NROM [data]
  Mapper
  (mapper-read-prg [this addr]
    (let [mem (:prg-data data)]
      (if (mirrored-addr? this addr)
        [(get mem addr 0) this]
        [(get mem (bit-and 0x3fff addr) 0) this])))

  (mapper-write-prg [this v addr] [v this])

  ;; TODO: Implement reading/writing from/to VRAM
  (mapper-read-chr [this addr] [nil this])
  (mapper-write-chr [this v addr] [v this]))

(defn nrom [data]
  (NROM. data))

