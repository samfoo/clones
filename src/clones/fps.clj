(ns clones.fps
  (:gen-class :main true)
  (:require [clones.nes :refer :all]))

;; (defn- run-to-frame [current last-frame]
;;   (let [rendered-frame (get-in last-frame [:ppu :frame-count])
;;         current-frame (get-in current [:ppu :frame-count])]
;;     (if (= rendered-frame current-frame)
;;       (recur (system-step current) last-frame)
;;       current)))

(defmacro now []
  `(System/currentTimeMillis))

(defn- run-and-display-fps [nes]
  (let [start (now)
        next-frame (step-frame nes)
        elapsed (- (now) start)
        fps (* 1000 (/ 1 elapsed))]
    (print (format "\r%-8.2f fps" (float fps)))
    (flush)
    (recur next-frame)))

(defn -main [& args]
  (let [rom (first args)
        nes (init-nes rom)]
    (run-and-display-fps nes)))

