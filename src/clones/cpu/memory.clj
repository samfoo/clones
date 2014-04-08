(ns clones.cpu.memory
  (:require [clojure.algo.monads :refer :all]
            [clones.nes.memory   :refer :all]
            [clones.byte         :refer :all]))

(defn io-read [addr]
  (fn [machine]
    (mem-read machine addr)))

(defn io-write [v addr]
  (fn [machine]
    (mem-write machine v addr)))

(defn io-write-word [v addr]
  (let [high (high-byte v)
        low (low-byte v)]
    (domonad state-m
             [a (io-write high (inc addr))
              b (io-write low addr)]
             b)))

(defn io-read-word [addr]
  (domonad state-m
           [high (io-read (inc addr))
            low (io-read addr)]
           (bit-or (bit-shift-left high 8) low)))

(defmacro with-io-> [steps expr]
  `(domonad state-m ~steps ~expr))


(defmacro io-> [machine & steps]
  `(reduce
     (fn [~'mem ~'step]
       (~'step (second ~'mem)))
     [nil ~machine]
     [~@steps]))

(defmacro io-debug-> [machine & steps]
  `(first
     (reduce
       (fn [~'mem ~'step]
         (~'step (second ~'mem)))
       [nil ~machine]
       [~@steps])))

