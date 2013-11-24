(ns clones.nes.nestest
  (:gen-class :main true)
  (:require [clojure.java.io   :refer :all]
            [clansi            :refer :all]
            [clones.cpu        :refer :all]
            [clones.nes        :refer :all]
            [clones.nes.rom    :refer :all]
            [clones.cpu.debug  :refer :all]
            [clones.cpu.memory :refer :all]))

(defn- lazy-debug [nes]
  (cons (debug-step nes) (lazy-seq (lazy-debug (step nes)))))

(defn read-nintendulator-log [f]
  (with-open [rdr (reader f)]
    (doall (map #(clojure.string/join "" (take 81 %)) (line-seq rdr)))))

(defn pad-lengths [a b]
  (let [expected-size (max (count a) (count b))
        padded-a (str a (clojure.string/join "" (repeat (- expected-size (count a)) " ")))
        padded-b (str b (clojure.string/join "" (repeat (- expected-size (count b)) " ")))]
    [padded-a padded-b]))


(defn color-diff [a b color]
  (let [[padded-a padded-b] (pad-lengths a b)]
    (clojure.string/join "" (map
               (fn [achar bchar]
                 (if (not= achar bchar)
                   (style (str achar) color)
                   achar))
               padded-a padded-b))))

(defn pretty-diff [expected actual]
  [(color-diff expected actual :green) (color-diff actual expected :red)])

(defn- get-context [line-no]
  (let [log (read-nintendulator-log "assets/nestest.log")
        context (take (min line-no 15) (drop (- line-no 15) log))]
    (map #(str "                " %) context)))

(defn -main [& args]
  (let [machine (init-nes "assets/nestest.nes")]
    (doseq [[expected actual line] (map vector
                                        (read-nintendulator-log "assets/nestest.log")
                                        (lazy-debug (assoc machine :pc 0xc000))
                                        (range))]
      (if (not= expected actual)
        (let [[e a] (pretty-diff expected actual)]
          (println (clojure.string/join "\n" (get-context line)))
          (println (format "%-5d expected: %s" line e))
          (println (format "%-5d actual  : %s" line a))
          (System/exit 1))
        nil))
    (println "All systems nominal")))
