(ns clones.cpu-timing-spec
  (:require [speclj.core           :refer :all]
            [clones.cpu            :refer :all]
            [clones.cpu.memory     :refer :all]
            [clones.cpu.addressing :refer :all]))

(def cpu (make-cpu {}))
(def cpu-with-carry (assoc cpu :p carry-flag))
(def cpu-with-zero (assoc cpu :p zero-flag))
(def cpu-with-negative (assoc cpu :p negative-flag))
(def cpu-with-decimal (assoc cpu :p decimal-flag))
(def cpu-with-interrupt (assoc cpu :p interrupt-flag))
(def cpu-with-overflow (assoc cpu :p overflow-flag))

(describe "The timing of instructions on the NES's 6502 2A03/7 CPU"
  (describe "execute-with-timing"
    (describe "when an address mode crosses a page"
      (def will-cross
        (let [with-registers (merge cpu {:x 0xff
                                         :y 0xff})]
          (second (io-> with-registers
                        (io-write-word 0x00f0 0)
                        (io-write-word 0xf0 0xf0)))))

      (def timings {0x7d 5 0x79 5 0x71 6 0x3d 5 0x39 5 0x31 6 0xdd 5 0xd9 5
                    0xd1 6 0x5d 5 0x59 5 0x51 6 0xbd 5 0xb9 5 0xb1 6 0xbe 5
                    0xbc 5 0x1d 5 0x19 5 0x11 6 0xfd 5 0xf9 5 0xf1 6})

      (for [[op-code expected] timings]
        (let [op (get op-codes op-code)
              n (:name (meta op))]
          (describe (format "$%02x (%s)" op-code n)
            (it (str "should take " expected " cycles when crossing a page")
              (let [[cycles _] (execute-with-timing will-cross op)]
                (should= expected cycles)))))))


    (describe "branching instructions"
      (def timings {0x90 [cpu cpu-with-carry]
                    0xb0 [cpu-with-carry cpu]
                    0xf0 [cpu-with-zero cpu]
                    0x30 [cpu-with-negative cpu]
                    0xd0 [cpu cpu-with-zero]
                    0x10 [cpu cpu-with-negative]
                    0x50 [cpu cpu-with-overflow]
                    0x70 [cpu-with-overflow cpu]})

      (for [[op-code [branch-cpu no-branch-cpu]] timings]
        (let [op (get op-codes op-code)
              n (:name (meta op))]
          (describe (format "$%02x (%s)" op-code n)
            (it "should take 4 cycles when branching to a new page"
              (let [will-cross (second (io-> branch-cpu
                                             (io-write 0x90 0)))
                    [cycles _] (execute-with-timing will-cross op)]
                (should= 4 cycles)))

            (it "should take 2 cycles when not branching"
              (let [[cycles _] (execute-with-timing no-branch-cpu op)]
                (should= 2 cycles)))

            (it "should take 3 cycles when branching"
              (let [[cycles _] (execute-with-timing branch-cpu op)]
                (should= 3 cycles)))))))

    (describe "without page-crossing or branching"
      (def timings {0x69 2 0x65 3 0x75 4 0x6d 4 0x7d 4 0x79 4 0x61 6 0x71 5
                    0x29 2 0x25 3 0x35 4 0x2d 4 0x3d 4 0x39 4 0x21 6 0x31 5
                    0x0a 2 0x06 5 0x16 6 0x0e 6 0x1e 7 0x24 3 0x2c 4 0x00 7
                    0x18 2 0xd8 2 0x58 2 0xb8 2 0xc9 2 0xc5 3 0xd5 4 0xcd 4
                    0xdd 4 0xd9 4 0xc1 6 0xd1 5 0xe0 2 0xe4 3 0xec 4 0xc0 2
                    0xc4 3 0xcc 4 0xc6 5 0xd6 6 0xce 6 0xde 7 0xca 2 0x88 2
                    0x49 2 0x45 3 0x55 4 0x4d 4 0x5d 4 0x59 4 0x41 6 0x51 5
                    0xe6 5 0xf6 6 0xee 6 0xfe 7 0xe8 2 0xc8 2 0x4c 3 0x6c 5
                    0x20 6 0x60 6 0x40 6 0xa9 2 0xa5 3 0xb5 4 0xad 4 0xbd 4
                    0xb9 4 0xa1 6 0xb1 5 0xa2 2 0xa6 3 0xb6 4 0xae 4 0xbe 4
                    0xa0 2 0xa4 3 0xb4 4 0xac 4 0xbc 4 0x4a 2 0x46 5 0x56 6
                    0x4e 6 0x5e 7 0xea 2 0x09 2 0x05 3 0x15 4 0x0d 4 0x1d 4
                    0x19 4 0x01 6 0x11 5 0x48 3 0x08 3 0x68 4 0x28 4 0x2a 2
                    0x26 5 0x36 6 0x2e 6 0x3e 7 0x6a 2 0x66 5 0x76 6 0x6e 6
                    0x7e 7 0xe9 2 0xe5 3 0xf5 4 0xed 4 0xfd 4 0xf9 4 0xe1 6
                    0xf1 5 0x38 2 0xf8 2 0x78 2 0x85 3 0x95 4 0x8d 4 0x9d 5
                    0x99 5 0x81 6 0x91 6 0x86 3 0x96 4 0x8e 4 0x84 3 0x94 4
                    0x8c 4 0xaa 2 0xa8 2 0xba 2 0x8a 2 0x9a 2 0x98 2})

      (for [[op-code expected] timings]
        (let [op (get op-codes op-code)
              n (:name (meta op))]
          (it (str "should take " expected " cycles when executing op code "
                (format "$%02x" op-code) " (" n ")")
            (let [[cycles _] (execute-with-timing cpu op)]
              (should= expected cycles))))))))

