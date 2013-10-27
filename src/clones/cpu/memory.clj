(ns clones.cpu.memory
  (:require [clojure.string      :as str]
            [clojure.algo.monads :refer :all]
            [clones.byte         :refer :all]))

(defprotocol Device
  "A memory mapped I/O device that can be read from or written to."
  (device-read [m addr] "Reads a single byte from the device")
  (device-write [m v addr] "Writes a single byte to the device and returns the
                            mutated device (or a new instance)"))

(extend-protocol Device
  clojure.lang.Associative
  (device-read [device addr] [(get device addr 0) device])
  (device-write [device v addr] [v (assoc device addr v)]))

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
      (conj mounts m))))

(defn error-if-invalid-addr [mounts addr]
  (if (not-any? #(mount-contains? % addr) mounts)
    (throw (Error. (format "No device is mounted at 0x%04X, current devices: [%s]" addr (mounts-str mounts))))
    nil))

(defn mounts-write [mounts v addr]
  (error-if-invalid-addr mounts addr)
  (let [mounts-after-write (map
                             (fn [m]
                               (if (mount-contains? m addr)
                                 (let [device (:device m)
                                       relative-addr (- addr (:start m))
                                       [_ device-after-write] (device-write device v relative-addr)]
                                   (assoc m :device device-after-write))
                                 m))
                             mounts)]
    [v mounts-after-write]))

(defn mounts-read [mounts addr]
  (error-if-invalid-addr mounts addr)
  (reduce (fn [[result mounts-state] m]
            (if (mount-contains? m addr)
              (let [relative-addr (- addr (:start m))
                    [v device-after-read] (device-read (:device m) relative-addr)
                    mount-after-read (assoc m :device device-after-read)]
                [v (conj mounts-state mount-after-read)])
              [result (conj mounts-state m)])) [nil []] mounts))

(defn mem-write [dev v addr]
  (assoc dev :memory (mounts-write (:memory dev)
                                    v
                                    addr)))

(defn io-mount [dev start-addr end-addr mountable]
  (assoc dev :memory
         (mount-device (:memory dev)
                       start-addr end-addr
                       mountable)))

(defn io-read [addr]
  (fn [dev]
    (let [[v mounts-after-read] (mounts-read (:memory dev) addr)]
      [v (assoc dev :memory mounts-after-read)])))

(defn io-write [v addr]
  (fn [dev]
    (let [[v mounts-after-write] (mounts-write (:memory dev) v addr)]
      [v (assoc dev :memory mounts-after-write)])))

(defn io-write-word [v addr]
  (let [high (high-byte v)
        low (low-byte v)]
    (domonad state-m
             [a (io-write high (+ 1 addr))
              b (io-write low addr)]
             b)))

(defn io-read-word [addr]
  (domonad state-m
           [high (io-read (+ 1 addr))
            low (io-read addr)]
           (bit-or (bit-shift-left high 8) low)))

(defmacro with-io-> [steps expr]
  `(domonad state-m ~steps ~expr))

(defmacro io-> [dev & steps]
  `(reduce
     (fn [~'mem ~'step]
       (~'step (second ~'mem)))
     [nil ~dev]
     [~@steps]))

(defmacro io-debug-> [dev & steps]
  `(first
     (reduce
       (fn [~'mem ~'step]
         (~'step (second ~'mem)))
       [nil ~dev]
       [~@steps])))

