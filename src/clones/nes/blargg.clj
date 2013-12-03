(ns clones.nes.blargg
  (:gen-class :main true)
  (:require [clojure.java.io   :refer :all]
            [clones.cpu        :refer :all]
            [clones.nes        :refer :all]
            [clones.nes.rom    :refer :all]
            [clones.cpu.debug  :refer :all]
            [clones.cpu.memory :refer :all]))

(defn- await-test-start [nes]
  (let [cpu (:cpu nes)]
    (if (= 0x80 (io-debug-> cpu (io-read 0x6000)))
      nes
      (recur (system-step nes)))))

(defn- read-null-term-str-from [nes addr]
  (let [cpu (:cpu nes)
        b (io-debug-> cpu (io-read addr))
        c (when-not (zero? b)
            (char b))]
    (if (nil? c)
      ""
      (str c (read-null-term-str-from nes (inc addr))))))

(defn- current-result-text [nes]
  (read-null-term-str-from nes 0x6004))

(defn- await-test-finish [nes i]
  (let [cpu (:cpu nes)
        status (io-debug-> cpu (io-read 0x6000))]
    (if (= status 0x80)
      (recur (system-step nes) (inc i))
      nes)))

(defn -main [& args]
  (doseq [rom args]
    (time
      (let [nes (init-nes rom)
            pre-test-nes (await-test-start nes)
            result-state (await-test-finish pre-test-nes 0)]
        (println (current-result-text result-state))))))

