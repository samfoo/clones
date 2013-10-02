(ns clones.memory
  (:require [clojure.pprint :refer :all]))

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

(defn mounts-str [mounts]
  (pr-str (map #(select-keys % [:start, :end, :name]) mounts)))

(defn mount-device
  [mounts start-addr end-addr read-fn write-fn]
  (let [m {:start start-addr :end end-addr
           :read read-fn :write write-fn}
        overlaps? (mount-exists? mounts m)]
    (if overlaps?
      (throw (Error.  (format "Device already mounted, current devices %s" (mounts-str mounts))))
      (concat mounts [m]))))

(defn mount-find [mounts addr]
  (first
    (take-while #(mount-contains? % addr) mounts)))

(defn mount-read [mounts addr]
  (let [mount (mount-find mounts addr)]
    (if (nil? mount)
      (throw (Error. (format "No device is mounted to handle 0x%04X, current devices %s" addr (mounts-str mounts))))
      ((:read mount) (- addr (:start mount))))))

