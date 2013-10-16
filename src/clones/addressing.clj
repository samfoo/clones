(ns clones.addressing
  (:require [clones.cpu  :refer :all]
            [clones.byte :refer :all]))

(defprotocol AddressMode
  "A set of methods for accessing various address modes for reading/writing."
  (mode-addr [am cpu] (str "Return the address to read/write from for this "
                           "mode in the current CPU state or nil if not an "
                           "address"))
  (mode-read [am cpu] "Read a single byte from a CPU using this addressing mode")
  (mode-write [am cpu v] "Write a single byte to a CPU using this addressing mode"))

(defmacro defaddrmode [mode-name desc]
  (let [klass (symbol (str (name mode-name) "-mode"))]
    (if (keyword? desc)
      `(do
         (deftype ~klass [])
         (extend-protocol clones.addressing/AddressMode
           ~klass
           (~'mode-addr [~'this ~'cpu] nil)
           (~'mode-read [~'this ~'cpu] (~desc ~'cpu))
           (~'mode-write [~'this ~'cpu ~'v] (assoc ~'cpu ~desc ~'v)))
         (def ~mode-name (new ~klass)))
      `(do
         (deftype ~klass [])
         (extend-protocol clones.addressing/AddressMode
           ~klass
           (~'mode-addr [~'this ~'cpu] ~desc)
           (~'mode-read [~'this ~'cpu] (cpu-read ~'cpu (~'mode-addr ~'this ~'cpu)))
           (~'mode-write [~'this ~'cpu ~'v] (cpu-write ~'cpu ~'v (~'mode-addr ~'this ~'cpu))))
         (def ~mode-name (new ~klass))))))

(defaddrmode implied
  (do
    (throw (Error. "Can't read/write to the implied address mode"))))

(defaddrmode accumulator :a)
(defaddrmode immediate (:pc cpu))

(defn zero-page-reg [cpu reg]
  (unsigned-byte
    (+
      (reg cpu)
      (cpu-read cpu (:pc cpu)))))

(defaddrmode zero-page (cpu-read cpu (:pc cpu)))
(defaddrmode zero-page-x (zero-page-reg cpu :x))
(defaddrmode zero-page-y (zero-page-reg cpu :y))

(defaddrmode relative
  (let [offset (cpu-read cpu (:pc cpu))]
    (if (< offset 0x80)
      (+ 1 (:pc cpu) offset)
      (+ 1 (- (:pc cpu) 0x100) offset))))

(defaddrmode absolute (cpu-read-word cpu (:pc cpu)))
(defaddrmode absolute-x (+ (:x cpu) (mode-addr absolute cpu)))
(defaddrmode absolute-y (+ (:y cpu) (mode-addr absolute cpu)))

(defaddrmode indirect
  (let [abs (mode-addr absolute cpu)
        abs-wrapped (if (= 0xff (unsigned-byte abs))
                      (bit-and abs 0xff00)
                      (inc abs))]
    (cpu-read-word cpu abs-wrapped)))

(defaddrmode indexed-indirect
  (let [pointer (cpu-read cpu (:pc cpu))]
    (cpu-read-word cpu (+ pointer (:x cpu)))))

(defaddrmode indirect-indexed
  (let [pointer (cpu-read cpu (:pc cpu))]
    (+ (cpu-read-word cpu pointer) (:y cpu))))
