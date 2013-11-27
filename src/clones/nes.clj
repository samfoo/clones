(ns clones.nes
  (:require [clones.cpu         :refer :all]
            [clones.ppu         :refer :all]
            [clones.nes.rom     :refer :all]
            [clones.nes.memory  :refer :all]
            [clones.cpu.debug   :refer :all]
            [clones.cpu.memory  :refer :all]))

(defrecord NES [cpu
                ppu
                apu
                mapper
                interrupt-requests])

(defn system-step [nes]
  (let [[cpu-cycles new-machine] (cpu-step nes)]
    new-machine))

(defn- make-nes [cpu ppu apu mapper]
  (NES. cpu ppu apu mapper #{}))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        mapper (make-mapper rom)
        ppu {}
        apu {0x04 0xff
             0x05 0xff
             0x06 0xff
             0x07 0xff
             0x15 0xff}
        cpu (make-cpu (make-main-memory ppu apu mapper))
        [reset-vector _] (io-> cpu (io-read-word 0xfffc))
        cpu-ready (assoc cpu :pc reset-vector)]
    (make-nes cpu-ready ppu apu mapper)))

