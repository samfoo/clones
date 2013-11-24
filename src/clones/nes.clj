(ns clones.nes
  (:require [clones.cpu         :refer :all]
            [clones.nes.rom     :refer :all]
            [clones.nes.memory  :refer :all]
            [clones.cpu.debug   :refer :all]
            [clones.cpu.memory  :refer :all]))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        mapper (make-mapper rom)
        cpu (make-cpu (make-memory {} {0x04 0xff
                                       0x05 0xff
                                       0x06 0xff
                                       0x07 0xff
                                       0x15 0xff} mapper))
        [reset-vector _] (io-> cpu (io-read-word 0xfffc))
        cpu-ready (assoc cpu :pc reset-vector)]
    cpu-ready))

