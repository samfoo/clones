(ns clones.byte)

(defn unsigned-byte [b] (bit-and 0xff b))
(defn bit-set? [x mask] (= (bit-and x mask) mask))

(defn high-byte [word] (unsigned-byte (bit-shift-right word 8)))
(defn low-byte [word] (unsigned-byte word))
