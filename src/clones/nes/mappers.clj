(ns clones.nes.mappers
  (:require [clones.cpu.memory :refer :all]))

(defprotocol Mapper
  "A mapper is a piece of hardware soldered to a
  cartridge's printed circuit board that performs address
  decoding, bank switching, and possibly other tasks.
  Some mappers can generate interrupts for timing; some
  Famicom games' mappers even have extra audio channels."

  (mapper-read-prg [device addr] "Reads a single byte from program ROM/RAM")
  (mapper-write-prg [device v addr] "Writes a single byte to program RAM")
  (mapper-read-chr [device addr] "Reads a single byte from video ROM/RAM")
  (mapper-write-chr [device v addr] "Writes a single byte to video RAM"))

(deftype MapperPrgDevice [mapper]
  Device
  (device-read [this addr]
    (let [[v after-read] (mapper-read-prg mapper addr)]
      [v (MapperPrgDevice. after-read)]))

  (device-write [this v addr]
    (let [[_ after-write] (mapper-write-prg mapper v addr)]
      [v (MapperPrgDevice. after-write)])))

(defn mapper-prg-device [mapper] (MapperPrgDevice. mapper))
