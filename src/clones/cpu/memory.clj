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
  (device-read [this addr] [(get this addr 0) this])
  (device-write [this v addr] [v (assoc this addr v)]))

(defn mounts-overlap? [m1 m2]
  (and
    (<= (:start m1) (:end m2))
    (<= (:start m2) (:end m1))))

(defn mount-exists? [mounts m]
  (some #(mounts-overlap? m %) mounts))

(defmacro mount-contains? [mount addr]
  `(and
     (<= (:start ~mount) ~addr)
     (>= (:end ~mount) ~addr)))

(defn mount-str [mount]
  (pr-str (merge (select-keys mount [:start :end]) (meta mount))))

(defn mounts-str [mounts]
  (str/join ", " (map mount-str (vals mounts))))

(defn mount-device
  [mounts dev-name start-addr end-addr device]
  (let [m {:start start-addr :end end-addr :device device}
        new-meta (assoc (meta mounts) dev-name {:start start-addr
                                                :end end-addr})
        overlaps? (mount-exists? (vals mounts) m)
        has-name? (contains? mounts dev-name)]
    (cond
      overlaps?  (throw (Error.
                          (format "Device already mounted at 0x%04X to 0x%04x, current devices: [%s]"
                                  start-addr end-addr
                                  (mounts-str mounts))))
      has-name? (throw (Error.
                         (format "Duplicate mount name %s, current devices: [%s]"
                                 dev-name
                                 (mounts-str mounts))))
      :else (with-meta (assoc mounts dev-name m) new-meta))))

(defn- mount-write-rel [m v addr]
  (let [device (:device m)
        relative-addr (- addr (:start m))
        [_ device-after-write] (device-write device v relative-addr)]
    (assoc m :device device-after-write)))

(defn- no-device-error [addr mounts]
  (throw (Error. (format "No device is mounted at 0x%04X, current devices: [%s]" addr (mounts-str mounts)))))

(defn- mount-name-for-addr [mounts-meta addr]
  (let [mount-pair (first (filter #(mount-contains? (second %) addr) mounts-meta))]
    (if (nil? mount-pair)
      (no-device-error addr mounts-meta)
      (first mount-pair))))

(def mount-name-for-addr-memo (memoize mount-name-for-addr))

(defn- mounts-write-abs [mounts v addr]
  (let [k (mount-name-for-addr-memo (meta mounts) addr)
        m (k mounts)]
    (assoc mounts k (mount-write-rel m v addr))))

(defn mounts-write [mounts v addr]
  (let [mounts-after-write (mounts-write-abs mounts v addr)]
    [v mounts-after-write]))

(defn mounts-read [mounts addr]
  (let [k (mount-name-for-addr-memo (meta mounts) addr)
        m (k mounts)
        relative-addr (- addr (:start m))
        [v device-after-read] (device-read (:device m) relative-addr)
        after-read (assoc m :device device-after-read)]
    [v (assoc mounts k after-read)]))

(defn mem-write [dev v addr]
  (assoc dev :memory (mounts-write (:memory dev)
                                    v
                                    addr)))

(defn io-mount [machine dev-name start-addr end-addr mountable]
  (assoc machine :memory
         (mount-device (:memory machine)
                       dev-name
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

