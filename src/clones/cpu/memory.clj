(ns clones.cpu.memory
  (:require [clojure.algo.monads :refer :all]
            [clones.device       :refer :all]
            [clones.byte         :refer :all]))

(defn io-read [addr]
  (fn [dev]
    (let [[v bus-after-read] (device-read (:memory dev) addr)]
      [v (assoc dev :memory bus-after-read)])))

(defn io-write [v addr]
  (fn [dev]
    (let [[v bus-after-write] (device-write (:memory dev) v addr)]
      [v (assoc dev :memory bus-after-write)])))

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

