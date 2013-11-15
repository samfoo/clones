(ns clones.nes.rom
  (:use [clones.byte             :refer :all]
        [clones.nes.mappers.nrom :refer :all]
        [clojure.java.io         :only [file input-stream]]))

(def mapper-names {
    0   "NROM",
    1   "SxROM, MMC1",
    2   "UxROM",
    3   "CNROM",
    4   "TxROM, MMC3, MMC6",
    5   "ExROM, MMC5",
    7   "AxROM",
    9   "PxROM, MMC2",
    10  "FxROM, MMC4",
    11  "Color Dreams",
    13  "CPROM",
    15  "100-in-1 Contra Function 16",
    16  "Bandai EPROM (24C02)",
    18  "Jaleco SS8806",
    19  "Namco 163",
    21  "VRC4a, VRC4c",
    22  "VRC2a",
    23  "VRC2b, VRC4e",
    24  "VRC6a",
    25  "VRC4b, VRC4d",
    26  "VRC6b",
    34  "BNROM, NINA-001",
    64  "RAMBO-1",
    66  "GxROM, MxROM",
    68  "After Burner",
    69  "FME-7, Sunsoft 5B",
    71  "Camerica/Codemasters",
    73  "VRC3",
    74  "Pirate MMC3 derivative",
    75  "VRC1",
    76  "Namco 109 variant",
    79  "NINA-03/NINA-06",
    85  "VRC7",
    86  "JALECO-JF-13",
    94  "Senjou no Ookami",
    105 "NES-EVENT Similar to MMC1",
    113 "NINA-03/NINA-06??",
    118 "TxSROM, MMC3",
    119 "TQROM, MMC3",
    159 "Bandai EPROM (24C01)",
    166 "SUBOR",
    167 "SUBOR",
    180 "Crazy Climber",
    185 "CNROM with protection diodes",
    192 "Pirate MMC3 derivative",
    206 "DxROM, Namco 118 / MIMIC-1",
    210 "Namco 175 and 340",
    228 "Action 52",
    232 "Camerica/Codemasters Quattro",
})

(def mappers {0 nrom})

(defn make-mapper [rom]
  (let [mapper-id (:mapper rom)
        mapper-fn (get mappers mapper-id)]
    (if (nil? mapper-fn)
      (throw (ex-info "Unsupported mapper"
                      {:id mapper-id
                       :name (get mapper-names mapper-id "unknown")}))
      (mapper-fn rom))))

(defn- bit-mask [n]
  (if (> n 0)
    (bit-or
      (bit-shift-left 1 (dec n))
      (bit-mask (dec n)))
    0))

(defn- sum [s] (reduce + 0 s))

(defn- bit-val [b len offset]
  (let [shift-amt (- offset len)
        mask (bit-shift-left
               (bit-mask len)
               shift-amt)
        result (bit-shift-right
                 (bit-and b mask)
                 shift-amt)]
    (if (= 1 len)
      (= result 1)
      result)))

(defn- bit-seq [b bit-lengths]
  (map-indexed
    (fn [i len]
      (let [offset (- 8 (sum (take i bit-lengths)))]
        (bit-val b len offset)))
    bit-lengths))

(defn- bit-map [b & desc]
  (let [ks (map first (partition 2 desc))
        vs (map second (partition 2 desc))]
    (zipmap ks (bit-seq b vs))))

(def prg-bank-size 16384)
(def chr-bank-size 8192)

(defn parse-ines-header [data]
  (let [sig (take 4 data)
        prg-rom-size (* prg-bank-size (nth data 4))
        prg-banks (nth data 4)
        chr-rom-size (* chr-bank-size (nth data 5))
        chr-banks (nth data 5)
        flags-6 (bit-map (nth data 6)
                         :lower-mapper-nibble 4
                         :mirroring-upper 1
                         :trainer-present? 1
                         :battery-backed? 1
                         :mirroring-lower 1)
        flags-7 (bit-map (nth data 7)
                         :upper-mapper-nibble 4
                         :nes-2 2
                         :playchoice-10? 1
                         :vs-unisystem? 1)
        mapper (bit-or (:lower-mapper-nibble flags-6)
                       (bit-shift-left
                         (:upper-mapper-nibble flags-7)
                         4))
        mirroring (if (:mirroring-upper flags-6)
                    :four-screen
                    (if (:mirroring-lower flags-6)
                      :vertical
                      :horizontal))
        nes-2? (= 2 (:nes-2 flags-7))]
    (if (not= sig [78 69 83 26])
      (throw (Error. "Invalid iNES file"))
      (merge {:prg-rom-size prg-rom-size
              :prg-banks prg-banks
              :chr-rom-size chr-rom-size
              :chr-banks chr-banks
              :mapper mapper
              :mapper-name (get mapper-names mapper)
              :mirroring mirroring
              :nes-2? nes-2?}

             (select-keys flags-6 [:trainer-present?
                                   :battery-backed?])

             (select-keys flags-7 [:playchoice-10?
                                   :vs-unisystem?])))))

(defn file-data [filename]
  (with-open [f (input-stream filename)]
    (let [size (.length (file filename))
          ba (byte-array size)]
      (.read f ba)
      (map unsigned-byte (sequence ba)))))

(def header-size 16)
(def trainer-size 512)

(defn read-rom [filename]
  (let [data (file-data filename)
        header-data (take header-size data)
        header (parse-ines-header header-data)
        prg-offset (if (:trainer-present? header)
                     (+ header-size trainer-size)
                     header-size)
        prg-data (->> data
                   (drop prg-offset)
                   (take (:prg-rom-size header)))
        chr-offset (if (:trainer-present? header)
                     (+ header-size trainer-size (:prg-rom-size header))
                     (+ header-size (:prg-rom-size header)))
        chr-data (->> data
                   (drop chr-offset)
                   (take (:chr-rom-size header)))]
    (merge header {:prg-data (vec prg-data)
                   :chr-data (vec chr-data)})))

