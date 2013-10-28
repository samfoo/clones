(ns clones.cpu.debug
  (:require [clones.cpu.memory     :refer :all]
            [clones.cpu            :refer :all]
            [clones.byte           :refer :all]
            [clones.cpu.addressing :refer :all]))

(defn- inc-pc [cpu]
  (assoc cpu :pc (inc (:pc cpu))))

(defn- op-code-arg [cpu mode]
  (let [c (inc-pc cpu)]
    (condp = (mode-size mode)
      0 nil
      1 (io-debug-> c (io-read (:pc c)))
      2 (io-debug-> c (io-read-word (:pc c))))))

(defn debug-ops-argument [cpu mode]
  (condp = (mode-size mode)
    0 (format "%-5s" " ")
    1 (format "%02X   " (op-code-arg cpu mode))
    2 (format "%02X %02X"
              (io-debug-> cpu (io-read (inc (:pc cpu))))
              (io-debug-> cpu (io-read (+ 2 (:pc cpu)))))))

(defn debug-cpu-state [cpu]
  (format "A:%02X X:%02X Y:%02X P:%02X SP:%02X"
          (:a cpu)
          (:x cpu)
          (:y cpu)
          (:p cpu)
          (:sp cpu)))

(defn debug-address-mode [cpu mode op-name]
  (let [arg (op-code-arg cpu mode)
        location (if (not= mode implied)
                   (io-debug-> (inc-pc cpu) (mode))
                   nil)
        value (if (not= mode implied)
                (io-debug-> (inc-pc cpu) (mode-read mode))
                nil)]
  (condp = mode
    immediate (format "#$%02X" arg)
    zero-page (format "$%02X = %02X"
                      arg
                      value)
    zero-page-x (format "$%02X,X @ %02X = %02X"
                        arg
                        (unsigned-byte location)
                        value)
    zero-page-y (format "$%02X,Y @ %02X = %02X"
                        (op-code-arg cpu)
                        (unsigned-byte location)
                        value)
    indirect (format "($%04X) = %04X"
                     (io-debug-> (inc-pc cpu) (absolute))
                     location)
    absolute (case op-name
               ("jmp" "jsr") (format "$%04X" location)
               (format "$%04X = %-20.02X" location value))
    absolute-x (format "$%04X,X @ %04X = %02X"
                       (io-debug-> (inc-pc cpu) (absolute))
                       location
                       value)
    absolute-y (format "$%04X,Y @ %04X = %02X"
                       (io-debug-> (inc-pc cpu) (absolute))
                       location
                       value)
    indexed-indirect (format "($%02X,X) @ %02X = %04X = %02X"
                             arg
                             (+ arg (:x (inc-pc cpu)))
                             location
                             value)
    indirect-indexed (format "($%02X),Y = %04X @ %04X = %02X"
                             arg
                             (- location (:y (inc-pc cpu)))
                             location
                             value)
    relative (format "$%04X" location)
    accumulator (format "%s" "A")
    implied "")))

(defn debug-step [cpu]
  (let [[op-code after-read] (io-> cpu (io-read (:pc cpu)))
        op (get op-codes op-code)
        {:keys [address-mode name]} (meta op)]
    (format "%04X %02X %s %4s %-27s %s"
            (:pc cpu)
            op-code
            (debug-ops-argument cpu address-mode)
            (clojure.string/upper-case name)
            (debug-address-mode cpu address-mode name)
            (debug-cpu-state cpu))))

