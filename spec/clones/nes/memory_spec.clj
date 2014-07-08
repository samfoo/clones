(ns clones.nes.memory-spec
  (:require [speclj.core         :refer :all]
            [clones.nes.memory   :refer :all]
            [clones.nes.mappers  :refer :all]
            [clones.ppu          :as    ppu]
            [clones.device       :refer :all]
            [clojure.algo.monads :refer :all]))

(defrecord MockMapper [prg chr]
  Mapper
  (prg-read [this addr] [(get prg addr 0) this])
  (prg-write [this v addr] [v (assoc this :prg (assoc prg addr v))])
  (chr-read [this addr] [(get chr addr 0) this])
  (chr-write [this v addr] [v (assoc this :chr (assoc chr addr v))]))

(defn mapper [] (MockMapper. {} {}))
(defn stub-mapper [prg chr] (MockMapper. prg chr))
(def bus {:internal-ram {}
          :video-memory {}
          :apu {}
          :mapper (mapper)})

(describe
  "The NES's memory bus, mapped with devices like APU, PPU, and cartridge"
  (tags :memory)

  (defn should-return-machine-after-reading [addr]
    (it "should return the bus after reading"
      (should= bus (second (mem-read bus addr)))))

  (describe "$4020 - $ffff"
    (should-return-machine-after-reading 0x6000)

    (it "should write to the mapper"
      (let [after-write (second (mem-write bus 0xff 0x4020))]
        (should= 0xff (first (mem-read after-write 0x4020)))))

    (it "should read from the mapper"
      (let [machine-with-value (assoc bus
                                  :mapper (stub-mapper {0x6000 0xbe} {}))]
        (should= 0xbe (first (mem-read machine-with-value 0x6000))))))

  ;; (describe "$4014 where the written value is $xx"
  ;;   (should-return-machine-after-reading 0x4014)
  ;;
  ;;   (it "should read 256 bytes from $xx00 - $xxff and write them to $2014"
  ;;     (let [ppu (dma-ppu)
  ;;           machine-with-oam (merge bus
  ;;                               {:mapper (stub-mapper {0x4400 0x11 0x44ff 0xff} {})
  ;;                                :ppu ppu})
  ;;           [_ after-dma] (mem-write machine-with-oam 0x44 0x4014)]
  ;;       (should= 256 (count (:writes (:ppu after-dma))))
  ;;       (should= {4 0x11} (first (:writes (:ppu after-dma))))
  ;;       (should= {4 0xff} (last (:writes (:ppu after-dma)))))))

  (describe "$4000 - $4013, $4015"
    (should-return-machine-after-reading 0x4000)

    (it "should write to the APU at $4015"
      (let [after-write (second (mem-write bus 0xff 0x4015))]
        (should= 0xff (first (mem-read after-write 0x4015)))))

    (it "should read from the APU at $4015, relative to $4000"
      (let [machine-with-value (assoc bus
                                  :apu
                                  {0x15 0xbe})]
        (should= 0xbe (first (mem-read machine-with-value 0x4015)))))

    (it "should write to the APU"
      (let [after-write (second (mem-write bus 0xff 0x4000))]
        (should= 0xff (first (mem-read after-write 0x4000)))))

    (it "should read from the APU relative to $4000"
      (let [machine-with-value (assoc bus
                                  :apu
                                  {0 0xbe})]
        (should= 0xbe (first (mem-read machine-with-value 0x4000))))))

  (describe "$2000 - $3fff"
    (defn mock-ppu-read [b a] [(get (:video-memory b) a 0) b])
    (defn mock-ppu-write[b v a] [v (assoc-in b [:video-memory a] v)])

    (should-return-machine-after-reading 0x2000)

    (it "should mirror all addresses > $2007 to $2000 - $2007 when writing"
      (with-redefs [ppu/ppu-read mock-ppu-read
                    ppu/ppu-write mock-ppu-write]
        (let [after-write (second (mem-write bus 0xff 0x2008))]
          (should= 0xff (first (mem-read after-write 0x2000))))))

    (it "should write to the PPU"
      (with-redefs [ppu/ppu-read mock-ppu-read
                    ppu/ppu-write mock-ppu-write]
        (let [after-write (second (mem-write bus 0xff 0x2000))]
          (should= 0xff (first (mem-read after-write 0x2000))))))

    (it "should mirror all addresses > $2007 to $2000 - $2007 when reading"
      (with-redefs [ppu/ppu-read mock-ppu-read]
        (let [machine-with-value (assoc bus :video-memory {0 0xbe})]
          (should= 0xbe (first (mem-read machine-with-value 0x2008))))))

    (it "should read from the PPU relative to $2000"
      (with-redefs [ppu/ppu-read mock-ppu-read]
        (let [machine-with-value (assoc bus :video-memory {0 0xbe})]
          (should= 0xbe (first (mem-read machine-with-value 0x2000)))))))

  (describe "$0000 - $1fff"
    (should-return-machine-after-reading 0)

    (it "should mirror all addresses > $07ff to $0000 - $07ff when writing"
      (let [after-write (second (mem-write bus 0xff 0x800))]
        (should= 0xff (first (mem-read after-write 0)))))

    (it "should write to internal RAM"
      (let [after-write (second (mem-write bus 0xff 0))]
        (should= 0xff (first (mem-read after-write 0)))))

    (it "should mirror all addresses > $07ff to $0000 - $07ff when reading"
      (let [machine-with-value (assoc bus
                                  :internal-ram
                                  {0 0xbe})]
        (should= 0xbe (first (mem-read machine-with-value 0x800)))))

    (it "should read from internal RAM"
      (let [machine-with-value (assoc bus
                                  :internal-ram
                                  {0 0xbe})]
        (should= 0xbe (first (mem-read machine-with-value 0)))))))

