(ns clones.memory
  (:require [clojure.pprint :refer :all]
            [clojure.string :as str]))

(defprotocol Device
  "A memory mapped I/O device that can be read from or written to."
  (read-device [m addr] "Reads a single byte from the device")
  (write-device [m v addr] "Writes a single byte to the device and returns the
                            mutated device (or a new instance)"))

(extend-protocol Device
  (type {})
  (read-device [this addr] (get this addr 0))
  (write-device [this v addr] (assoc this addr v)))

(defn mounts-overlap? [m1 m2]
  (and
    (<= (:start m1) (:end m2))
    (<= (:start m2) (:end m1))))

(defn mount-contains? [mount addr]
  (and
    (<= (:start mount) addr)
    (>= (:end mount) addr)))

(defn mount-exists? [mounts m]
  (some #(mounts-overlap? m %) mounts))

(defn mount-str [mount]
  (pr-str (merge (select-keys mount [:start :end]) (meta mount))))

(defn mounts-str [mounts]
  (str/join ", " (map mount-str mounts)))

(defn mount-device
  [mounts start-addr end-addr device]
  (let [m {:start start-addr :end end-addr :device device}
        overlaps? (mount-exists? mounts m)]
    (if overlaps?
      (throw (Error.
               (format "Device already mounted at 0x%04X to 0x%04x, current devices: [%s]"
                       start-addr end-addr
                       (mounts-str mounts))))
      (concat mounts [m]))))

(defn mount-find [mounts addr]
  (first
    (take-while #(mount-contains? % addr) mounts)))

(defn mount-write [mounts v addr]
  (if (not-any? #(mount-contains? % addr) mounts)
    (throw (Error. (format "No device is mounted at 0x%04X, current devices: [%s]" addr (mounts-str mounts))))
    (map (fn [m]
           (let [device (:device m)
                 offset (- addr (:start m))]
             (if (mount-contains? m addr)
               (assoc m :device (write-device device v offset))
               m))) mounts)))

(defn mount-read [mounts addr]
  (let [mount (mount-find mounts addr)]
    (if (nil? mount)
      (throw (Error. (format "No device is mounted at 0x%04X, current devices: [%s]" addr (mounts-str mounts))))
      (read-device (:device mount) (- addr (:start mount))))))

