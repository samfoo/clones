(ns clones.nes.mappers.nrom
  (:require [clones.nes.mappers :refer :all]))

(deftype NROM [^int banks
               ^ints prg-rom]
  Mapper
  (mapper-read-prg [this addr]
    (if (and (> addr 0x3fff) (> banks 1))
      [(aget prg-rom addr) this]
      [(aget prg-rom (bit-and 0x3fff addr)) this]))

  (mapper-write-prg [this v addr] [v this])

  ;; TODO: Implement reading/writing from/to VRAM
  (mapper-read-chr [this addr] [nil this])
  (mapper-write-chr [this v addr] [v this]))

(defn nrom [rom]
  (NROM. (:prg-banks rom) (int-array (:prg-data rom))))

