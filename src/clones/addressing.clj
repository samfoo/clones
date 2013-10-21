(ns clones.addressing
  (:require [clones.memory       :refer :all]
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
             (if (< offset 0x80)
               (+ 1 (:pc cpu) offset)
               (+ 1 (- (:pc cpu) 0x100) offset))))

(defn absolute []
  (with-io-> [cpu (fetch-state)
              addr (io-read-word (:pc cpu))]
             addr))

(defn absolute-x []
  (with-io-> [cpu (fetch-state)
              abs-addr (absolute)]
             (+ (:x cpu) abs-addr)))

(defn absolute-y []
  (with-io-> [cpu (fetch-state)
              abs-addr (absolute)]
             (+ (:y cpu) abs-addr)))

(defn wrap-abs-addr [abs-addr]
  (if (= 0xff (unsigned-byte abs-addr))
    (bit-and abs-addr 0xff00)
    (inc abs-addr)))

(defn indirect []
  (with-io-> [abs-addr (absolute)
              addr (io-read-word (wrap-abs-addr abs-addr))]
             addr))

(defn indexed-indirect []
  (with-io-> [cpu (fetch-state)
              pointer (io-read (:pc cpu))
              addr (io-read-word (+ pointer (:x cpu)))]
             addr))

(defn indirect-indexed []
  (with-io-> [cpu (fetch-state)
              pointer (io-read (:pc cpu))
              ind-pointer (io-read-word pointer)]
             (+ ind-pointer (:y cpu))))

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

