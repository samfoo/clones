(ns clones.cpu.memory-spec
  (:require [speclj.core         :refer :all]
            [clones.cpu.memory   :refer :all]
            [clojure.algo.monads :refer :all]))

(def mounts {})
(deftype MutatesOnRead [mutated])
(extend-protocol Device
  MutatesOnRead
  (device-read [device addr] [0xff (new MutatesOnRead true)])
  (device-write [device v addr] [nil nil]))

(describe "Mountable memory"
  (describe "reading & writing"
    (let [mounts (mount-device mounts :ram 0x0000 0x0999 {0 0xff 1 0xee})
          xpu {:memory mounts}]
      (describe "monadic I/O"
        (describe "io-write"
          (it "should return the mutated device as the second element"
            (let [[_ new-xpu] ((domonad state-m [a (io-write 1 0)] a) xpu)
                  new-mounts (:memory new-xpu)
                  m (:ram new-mounts)
                  buffer (:device m)
                  written (get buffer 0 :should-be-1)]
              (should= 1 written)))

          (it "should return the written byte as the first element"
            (let [[result _] ((domonad state-m [a (io-write 1 0)] a) xpu)]
              (should= 1 result))))

        (describe "io-read-word"
          (it "should return the device as the second element"
            (let [[_ new-xpu] ((domonad state-m [a (io-read-word 0)] a) xpu)]
              (should= xpu new-xpu)))

          (it "should read two bytes starting at {addr} where the high byte is
               at {addr + 1} and the low byte is at {addr}, and return that
               word as the first element"
            (let [[result _] ((domonad state-m [a (io-read-word 0)] a) xpu)]
              (should= 0xeeff result))))

        (describe "io-read"
          (it "should return the mutated device as the second element if the
               device is something that changes on read (e.g. PPU)"
            (let [mutatable (new MutatesOnRead false)
                  mounts (mount-device {} :ram 0x0000 0x0999 mutatable)
                  mutating-xpu {:memory mounts}
                  [_ new-xpu] ((domonad state-m [a (io-read 0)] a) mutating-xpu)
                  new-mounts (:memory new-xpu)
                  m (:ram new-mounts)
                  mutated-device (:device m)
                  mutated? (.mutated mutated-device)]
              (should-not (.mutated mutatable))
              (should mutated?)))

          (it "should return the device as the second element"
            (let [[_ new-xpu] ((domonad state-m [a (io-read 0)] a) xpu)]
              (should= xpu new-xpu)))

          (it "should read a single byte and return it as the first element"
            (let [[result _] ((domonad state-m [a (io-read 0)] a) xpu)]
              (should= 0xff result))))))

    (describe "mounts-write"
      (it "should raise an error trying to write to a location with no device"
        (should-throw Error "No device is mounted at 0x0000, current devices: []"
          (mounts-write mounts 0xbe 0x0000)))

      (it "should write to the correct mount point"
        (let [initial (mount-device mounts :ram 0x0000 0x0999 {})
              [_ after-write] (mounts-write initial 0xbe 0x100)]
          (should= 0xbe (first (mounts-read after-write 0x100))))))

    (describe "mounts-read"
      (it "should raise an error trying to read from a location with no device"
        (should-throw Error "No device is mounted at 0x0000, current devices: []"
          (mounts-read mounts 0x0000)))

      (it "should read from the correct mount point"
        (let [new-mounts (mount-device mounts :ram 0x0000 0x0999 {1 :read-byte})]
          (should= :read-byte (first (mounts-read new-mounts 1)))))))

  (describe "mount-device"
    (it "should raise an error trying to mount a name that already has a device"
      (let [new-mounts (mount-device mounts :ram 0x0000 0x0999 {})]
        (should-throw Error "Duplicate mount name :ram, current devices: [{:end 2457, :start 0}]"
          (mount-device new-mounts :ram 0x1000 0x5000 {}))))

    (it "should raise an error trying to mount to a range that already has a device"
      (let [new-mounts (mount-device mounts :ram 0x0000 0x0999 {})]
        (should-throw Error "Device already mounted at 0x0999 to 0x5000, current devices: [{:end 2457, :start 0}]"
          (mount-device new-mounts :more-ram 0x0999 0x5000 {}))))

    (it "should mount a device with a start and end memory point"
      (let [new-mounts (mount-device mounts :ram 0x0000 0x0999 {})]
        (should= new-mounts {:ram {:start 0x0000
                                   :end 0x0999
                                   :device {}}})))))

