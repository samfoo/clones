(ns clones.device)

(defprotocol Device
  "A memory mapped I/O device that can be read from or written to."
  (device-read [m addr] "Reads a single byte from the device")
  (device-write [m v addr] "Writes a single byte to the device and returns the
                            mutated device (or a new instance)"))

(extend-protocol Device
  clojure.lang.Associative
  (device-read [this addr] [(get this addr 0) this])
  (device-write [this v addr] [v (assoc this addr v)]))

