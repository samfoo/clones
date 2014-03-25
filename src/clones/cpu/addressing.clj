(ns clones.cpu.addressing
  (:require [clones.cpu.memory   :refer :all]
            [clones.byte         :refer :all]
            [clojure.algo.monads :refer :all]))

(defn accumulator [] (fn [cpu] [(:a cpu) cpu]))

(defn immediate [] (fn [cpu] [(:pc cpu) cpu]))

(defn implied []
  (fn [_]
    (throw (Error. "Can't read/write to the implied address mode"))))

(defn zero-page []
  (with-io-> [cpu (fetch-state)
              addr (io-read (:pc cpu))]
             addr))

(defn zero-page-reg [reg]
  (with-io-> [zp-addr (zero-page)
              cpu (fetch-state)]
             (unsigned-byte (+ (reg cpu) zp-addr))))

(defn zero-page-x [] (zero-page-reg :x))
(defn zero-page-y [] (zero-page-reg :y))

(defn relative []
  (with-io-> [cpu (fetch-state)
              offset (io-read (:pc cpu))]
             (unsigned-word
               (if (< offset 0x80)
                 (+ 1 (:pc cpu) offset)
                 (+ 1 (- (:pc cpu) 0x100) offset)))))

(defn absolute []
  (with-io-> [cpu (fetch-state)
              addr (io-read-word (:pc cpu))]
             addr))

(defn absolute-x []
  (with-io-> [cpu (fetch-state)
              abs-addr (absolute)]
             (unsigned-word (+ (:x cpu) abs-addr))))

(defn absolute-y []
  (with-io-> [cpu (fetch-state)
              abs-addr (absolute)]
             (unsigned-word (+ (:y cpu) abs-addr))))

(defn- indirect-high-addr [abs-addr]
  (if (= 0xff (bit-and 0xff abs-addr))
    (bit-and abs-addr 0xff00)
    (inc abs-addr)))

(defn indirect []
  (with-io-> [abs-addr (absolute)
              high (io-read (indirect-high-addr abs-addr))
              low (io-read abs-addr)]
             (bit-or (bit-shift-left high 8) low)))

(defn indexed-indirect []
  (with-io-> [cpu (fetch-state)
              pointer (io-read (:pc cpu))
              high (io-read (unsigned-byte (+ pointer (:x cpu) 1)))
              low (io-read (unsigned-byte (+ pointer (:x cpu))))]
             (bit-or (bit-shift-left high 8) low)))

(defn indirect-indexed-addr []
  (with-io-> [cpu (fetch-state)
              pointer (io-read (:pc cpu))
              high (io-read (unsigned-byte (inc pointer)))
              low (io-read (unsigned-byte pointer))]
    (bit-or (bit-shift-left high 8) low)))

(defn indirect-indexed []
  (with-io-> [cpu (fetch-state)
              ind-addr (indirect-indexed-addr)]
             (unsigned-word (+ ind-addr (:y cpu)))))

(defn mode-by-name [n]
  (n {:zero-page zero-page
      :zero-page-x zero-page-x
      :zero-page-y zero-page-y
      :accumulator accumulator
      :implied implied
      :immediate immediate
      :absolute absolute
      :absolute-y absolute-y
      :absolute-x absolute-x
      :indirect indirect
      :relative relative
      :indexed-indirect indexed-indirect
      :indirect-indexed indirect-indexed}))

(defn mode-write-mem [mode v]
  (with-io-> [addr (mode)
              result (io-write v addr)]
             result))

(defn mode-read-mem [mode]
  (with-io-> [addr (mode)
              result (io-read addr)]
             result))

(defn mode-write-reg [reg v]
  (fn [cpu] [v (assoc cpu reg v)]))

(defn mode-read-reg [reg]
  (fn [cpu] [(reg cpu) cpu]))

(defn mode-read [mode]
  (if (= mode accumulator)
    (mode-read-reg :a)
    (mode-read-mem mode)))

(defn mode-write [mode v]
  (if (= mode accumulator)
    (mode-write-reg :a v)
    (mode-write-mem mode v)))

(defn mode-size [mode]
  (condp = mode
    immediate 1
    zero-page 1
    zero-page-x 1
    zero-page-y 1
    indexed-indirect 1
    indirect-indexed 1
    relative 1
    absolute 2
    absolute-x 2
    absolute-y 2
    indirect 2
    0))

