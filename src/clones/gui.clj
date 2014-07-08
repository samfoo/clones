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
  (let [rendered-frame-id (get @nes-atom :frame-count)
        machine-frame-id (get nes :frame-count)]
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
  (.drawImage g frame-buffer 0 0 256 240 nil))

(defn- paint [c g nes]
  (let [frame-buffer (get nes :background-frame-buffer)]
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

(defn- paint-pattern-table-tiles [c g tiles]
  (doseq [i (range 256)]
    (let [tile (nth tiles i)
          x (* 8 (mod i 16))
          y (* 8 (int (/ i 16)))]
      (paint-pattern-table-tile c g tile x y))))

(defn- paint-pattern-table [c g nes which-table]
  (let [tiles (pattern-table-tiles nes which-table)]
    (paint-pattern-table-tiles c g tiles)))

(defn- show-pattern-table-window! [nes]
  (let [pattern-table-left (canvas :id :pattern-table-left
                                   :preferred-size [(* 16 8 2) :by (* 16 8 2)]
                                   :paint (fn [c g]
                                            (graphics/scale g 2)
                                            (paint-pattern-table c g @nes :left)))

        pattern-table-right (canvas :id :pattern-table-right
                                    :preferred-size [(* 16 8 2) :by (* 16 8 2)]
                                    :paint (fn [c g]
                                             (graphics/scale g 2)
                                             (paint-pattern-table c g @nes :right)))

        pattern-tables (border-panel
                         :hgap 5
                         :east (border-panel
                                 :vgap 5
                                 :north (label
                                          :text "Left $0000"
                                          :halign :center)
                                 :south pattern-table-left)
                         :west (border-panel
                                 :vgap 5
                                 :north (label
                                          :text "Right $1000"
                                          :halign :center)
                                 :south pattern-table-right))

        pattern-tables-window (frame :title "Clones - Pattern Tables"
                                     :visible? true
                                     :resizable? false
                                     :content (border-panel
                                                :center pattern-tables
                                                :hgap 10
                                                :border 10))]
    (pack! pattern-tables-window)))


(defn -main [& args]
  (native!)
  (let [rom (first args)
        nes (atom (init-nes rom))

        screen (canvas :id :screen
                       :paint (fn [c g] (paint c g @nes))
                       :background :black)

        screen-window (frame :title "Clones"
                             :width 256
                             :height 256
                             :visible? true
                             :on-close :dispose
                             :content screen)]
    ;; (show-pattern-table-window! nes)
    (run-machine nes)
    (timer (fn [_] (repaint! screen) 20))))
