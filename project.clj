(defproject clones "0.1"
  :description "An NES emulator with style"
  :url "http://example.com/FIXME"
  :license {:name "GPLv3"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/algo.monads "0.1.4"]
                 [seesaw "1.4.4"]
                 [clansi "1.0.0"]]
  :profiles {:dev {:dependencies [[speclj "2.5.0"]]}

             :nestest {:main clones.nes.nestest
                       :uberjar-name "nestest.jar"}

             :blargg {:main clones.nes.blargg
                      :aot :all
                      :uberjar-name "blargg-tester.jar"}

             :gui {:main clones.gui
                   :aot :all
                   :uberjar-name "gui.jar"}}
  :plugins [[speclj "2.7.0"]]
  :test-paths ["spec"])

