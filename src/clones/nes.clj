(ns clones.nes
  (:require [clones.cpu         :refer :all]
            [clones.ppu         :refer :all]
            [clones.nes.rom     :refer :all]
            [clones.cpu.debug   :refer :all]
            [clones.cpu.memory  :refer :all])
  (:use [clones.nes.memory :only [make-main-memory]]
        [clones.ppu.memory :only [make-ppu-memory]]))

(defrecord NES [cpu
                ppu
                apu
                mapper
                interrupt-requests])

(defn- catch-ppu-up [nes cycles]
  (if (= 0 cycles)
    nes
    (catch-ppu-up (ppu-step nes) (dec cycles))))

(defn- handle-interrupt [nes]
  (condp = (:interrupt nes)
    :nmi (perform-nmi nes)
    nes))

(defn- update-ppu [machine ppu]
  (-> machine
    (assoc-in [:cpu :memory :ppu] ppu)
    (assoc :ppu ppu)))

(defn system-step [nes]
  (let [[cpu-cycles after-cpu] (cpu-step nes)
        after-ppu (-> after-cpu
                    (update-ppu (get-in after-cpu [:cpu :memory :ppu]))
                    (catch-ppu-up (* 3 cpu-cycles)))
        with-updated-ppu (update-ppu after-ppu (:ppu after-ppu))]
    with-updated-ppu))

(defn- make-nes [cpu ppu apu mapper]
  (NES. cpu ppu apu mapper #{}))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        mapper (make-mapper rom)
        ppu (make-ppu (make-ppu-memory mapper))
        apu {0x04 0xff
             0x05 0xff
             0x06 0xff
             0x07 0xff
             0x15 0xff}
        cpu (make-cpu (make-main-memory ppu apu mapper))
        [reset-vector _] (io-> cpu (io-read-word 0xfffc))
        cpu-ready (assoc cpu :pc reset-vector)]
    (make-nes cpu-ready ppu apu mapper)))

