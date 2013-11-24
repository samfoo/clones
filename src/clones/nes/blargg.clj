(ns clones.nes.blargg
  (:gen-class :main true)
  (:require [clojure.java.io   :refer :all]
            [clones.cpu        :refer :all]
            [clones.nes        :refer :all]
            [clones.nes.rom    :refer :all]
            [clones.cpu.debug  :refer :all]
            [clones.cpu.memory :refer :all]))

(defn- await-test-start [nes]
  (if (= 0x80 (io-debug-> nes (io-read 0x6000)))
    nes
    (recur (second (step nes)))))

(defn- read-null-term-str-from [nes addr]
  (let [b (io-debug-> nes (io-read addr))
        c (if (= 0 b)
            nil
            (char b))]
    (if (nil? c)
      ""
      (str c (read-null-term-str-from nes (inc addr))))))

(defn- current-result-text [nes]
  (read-null-term-str-from nes 0x6004))

(defn- await-test-finish [nes i]
  (let [status (io-debug-> nes (io-read 0x6000))]
    (if (= status 0x80)
      (recur (second (step nes)) (inc i))
      nes)))

(defn -main [& args]
  (doseq [rom args]
    (time
      (let [pre-test-nes (await-test-start (init-nes rom))
            result-state (await-test-finish pre-test-nes 0)]
        (println (current-result-text result-state))))))

