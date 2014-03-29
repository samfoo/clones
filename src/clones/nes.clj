(ns clones.nes
  (:require [clones.cpu         :refer :all]
            [clones.ppu         :refer :all]
            [clones.nes.rom     :refer :all]
            [clones.cpu.debug   :refer :all]
            [clojure.pprint     :refer :all]
            [clones.nes.memory  :refer :all])
  (:use [clones.ppu.memory :only [make-ppu-memory]]))

(defn- catch-ppu-up [nes cycles]
  (loop [system nes
         remaining-cycles cycles]
    (if (zero? remaining-cycles)
      system
      (recur (ppu-step system) (- remaining-cycles 1)))))

(defn- handle-interrupts [nes]
  (condp = (:interrupt nes)
    :nmi (perform-nmi nes)
    nes))

(defn system-step [nes]
  (let [after-interrupts (handle-interrupts nes)
        [cpu-cycles machine-after-cpu] (cpu-step after-interrupts)
        with-updated-ppu (catch-ppu-up machine-after-cpu (* 3 cpu-cycles))]
    with-updated-ppu))

(defn- make-nes [cpu ppu apu mapper]
  (merge cpu {:ppu ppu
              :apu apu
              :internal-ram {}
              :mapper mapper
              :interrupt nil}))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        mapper (make-mapper rom)
        ppu (make-ppu (make-ppu-memory mapper))
        apu {0x04 0xff
             0x05 0xff
             0x06 0xff
             0x07 0xff
             0x15 0xff}
        cpu (make-cpu)
        machine (make-nes cpu ppu apu mapper)
        reset-vector (first (mem-read-word machine 0xfffc))
        machine-ready (assoc machine :pc reset-vector)]
    machine-ready))

