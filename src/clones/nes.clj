(ns clones.nes
  (:gen-class :main true)
  (:require [clones.cpu :refer :all]
            [clones.nes.rom :refer :all]
            [clones.cpu.memory :refer :all]))

(defn init-nes [rom-file]
  (let [rom (read-rom rom-file)
        cpu (make-cpu)
        cpu-with-rom (-> cpu
                       (io-mount 0x8000 0xbfff (:prg-data))
                       (io-mount 0xc000 0xffff (:prg-data)))
        cpu-ready (assoc cpu-with-rom :pc 0xc000)]
    cpu-ready))


(defn -main [& args]
  (let [cpu (init-nes (first args))]
    (step cpu)))

