(ns clones.nes
  (:gen-class :main true)
  (:require [clones.cpu         :refer :all]
            [clones.nes.rom     :refer :all]
            [clones.nes.mappers :refer :all]
            [clones.cpu.debug   :refer :all]
            [clones.cpu.memory  :refer :all]))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        mapper (make-mapper rom)
        prg-device (mapper-prg-device mapper)
        cpu (make-cpu)
        cpu-with-rom (-> cpu
                       (io-mount :ppu 0x2000 0x3fff {})
                       ;; Fake APU just reads 0xff for all registers. Need this
                       ;; for nestest until an actual APU is implemented
                       (io-mount :apu 0x4000 0x401f {0x04 0xff
                                                     0x05 0xff
                                                     0x06 0xff
                                                     0x07 0xff
                                                     0x15 0xff})
                       (io-mount :prg-ram 0x6000 0x7fff {})
                       (io-mount :prg-rom 0x8000 0xffff prg-device))
        [reset-vector _] (io-> cpu-with-rom (io-read-word 0xfffc))
        cpu-ready (assoc cpu-with-rom :pc reset-vector)]
    cpu-ready))

