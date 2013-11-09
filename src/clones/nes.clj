(ns clones.nes
  (:gen-class :main true)
  (:require [clones.cpu        :refer :all]
            [clones.nes.rom    :refer :all]
            [clones.cpu.debug  :refer :all]
            [clones.cpu.memory :refer :all]))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        cpu (make-cpu)
        cpu-with-rom (-> cpu
                       (io-mount 0x2000 0x3fff {})
                       (io-mount 0x8000 0xbfff (:prg-data rom))
                       (io-mount 0xc000 0xffff (:prg-data rom)))
        cpu-ready (assoc cpu-with-rom :pc 0xc000)]
    cpu-ready))

