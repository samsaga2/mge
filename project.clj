(defproject mge "0.1.0-SNAPSHOT"
  :description "MSX Game Engine"
  :url "https://github.com/samsaga2/mge"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-z80/clj-z80 "0.1.0-SNAPSHOT"]
                 [instaparse "1.4.10"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/tools.cli "0.4.2"]
                 [net.mikera/imagez "0.12.0"]]
  :jvm-opts []
  :main mge.core
  :aot [mge.core])
