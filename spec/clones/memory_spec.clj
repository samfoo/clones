(ns clones.cpu-spec
  (:require [speclj.core   :refer :all]
            [clones.memory :refer :all]))

(def mounts [])

(describe "Mountable memory"
  (describe "mount-read"
    (it "should translate the address passed to the device's read function to be relative to the mount point"
      (let [new-mounts (mount-device mounts 0x1000 0x1999 {})]
        (should= 0 (mount-read new-mounts 0x1000))))

    (it "should raise an error trying to read from a location with no device"
      (should-throw Error "No device is mounted at 0x0000, current devices: []"
        (mount-read mounts 0x0000))))

    (it "should read from the correct mount point"
      (let [new-mounts (mount-device mounts 0x0000 0x0999 {1 :read-byte})]
        (should= :read-byte (mount-read new-mounts 1)))))

  (describe "mount-find"
    (it "should return nil if no mount matches the address"
      (should-be-nil (mount-find mounts 0x0)))

    (it "should return the io functions when the addr is in its range"
      (let [new-mounts (mount-device mounts 0x0000 0x0999 {})]
        (should= (mount-find new-mounts 0x100) {:start 0x0000 :end 0x0999 :device {}}))))

  (describe "mount-device"
    (it "should raise an error trying to mount to a range that already has a device"
      (let [new-mounts (mount-device mounts 0x0000 0x0999 {})]
        (should-throw Error "Device already mounted at 0x0999 to 0x5000, current devices: [{:end 2457, :start 0}]"
          (mount-device new-mounts 0x0999 0x5000 {}))))

    (it "should mount a device with a start and end memory point"
      (let [new-mounts (mount-device mounts 0x0000 0x0999 {})]
        (should= new-mounts '({:start 0x0000 :end 0x0999 :device {}})))))
