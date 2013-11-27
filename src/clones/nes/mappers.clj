(ns clones.nes.mappers)

(defprotocol Mapper
  "A mapper is a cartridge"
  (prg-read [this addr] "Read program data on the mapper")
  (prg-write [this v addr] "Write program data on the mapper")
  (chr-read [this addr] "Read graphics data on the mapper")
  (chr-write [this v addr] "Write graphics data on the mapper"))

