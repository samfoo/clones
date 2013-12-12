(ns clones.gui
  (:gen-class :main true)
  (:require [clones.nes       :refer :all]
            [clones.cpu.debug :refer :all]
            [clones.ppu.debug :refer :all]
            [seesaw.graphics  :as    graphics]
            [seesaw.core      :refer :all]
            [seesaw.bind      :as     b])
  (:import [java.awt Graphics2D RenderingHints]))

(defn- run [nes-atom nes]
  (let [rendered-frame-id (get-in @nes-atom [:ppu :frame-count])
        machine-frame-id (get-in nes [:ppu :frame-count])]
    (when (not= rendered-frame-id machine-frame-id)
      (reset! nes-atom nes))
    (recur nes-atom (system-step nes))))

(defn- run-forever [nes]
  (fn []
    (run nes @nes)))

(defn- run-machine [nes]
  (let [thread (Thread. (run-forever nes))]
    (.start thread)))

(defn- get-color [pixel]
  (condp = pixel
    0 :white
    1 :red
    2 :green
    3 :blue))

(defn render-buffer [g frame-buffer]
  (doseq [i (range (* 256 240))]
    (doto g
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_OFF))

    (graphics/draw g
     (graphics/rect (mod i 256) (int (/ i 256)) 1)
     (graphics/style :background (get-color (get frame-buffer i))))))

(defn- paint [c g nes]
  (let [frame-buffer (get-in nes [:ppu :background-frame-buffer])]
    (render-buffer g frame-buffer)))

(defn- paint-pattern-table-tile [c g tile x y]
  (doseq [row (range 8)
          col (range 8)]
    (let [pixel (nth (nth tile row) col)]
      (doto g
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_OFF))

      (graphics/draw g
        (graphics/rect (+ x col) (+ y row) 1)
        (graphics/style :background (get-color pixel))))))

(defn- paint-pattern-table-tiles [c g tiles x-offset y-offset]
  (doseq [i (range 256)]
    (let [tile (nth tiles i)
          x (+
              x-offset
              (mod i 16)
              (* 8 (mod i 16)))
          y (+
              y-offset
              (int (/ i 16))
              (* 8 (int (/ i 16))))]
      (paint-pattern-table-tile c g tile x y))))

(defn- paint-pattern-tables [c g nes]
  (let [left-tiles (pattern-table-tiles (:ppu nes) :left)
        right-tiles (pattern-table-tiles (:ppu nes) :right)]
    (paint-pattern-table-tiles c g left-tiles 0 0)
    (paint-pattern-table-tiles c g right-tiles 150 0)))

(defn -main [& args]
  (native!)
  (let [rom (first args)
        nes (atom (init-nes rom))

        screen (canvas :id :screen
                       :paint (fn [c g] (paint c g @nes))
                       :background :black)

        pattern-tables (canvas :id :pattern-tables
                               :paint (fn [c g] (paint-pattern-tables c g @nes))
                               :background :black)

        pattern-tables-window (frame :title "Clones - Pattern Tables"
                                     :width 294
                                     :height 200
                                     :visible? true
                                     :content pattern-tables)

        screen-window (frame :title "Clones"
                             :width 256
                             :height 256
                             :visible? true
                             :on-close :dispose
                             :content screen)]
    (run-machine nes)
    (b/bind
      nes
      (b/b-do [_]
        (repaint! pattern-tables)
        (repaint! screen)))))
