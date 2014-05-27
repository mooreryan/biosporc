(defproject biosporc_new "0.1.0-SNAPSHOT"
  :description "New version of BioSPORC"
  :url "Let's hunt some chimeras."
  :license {:name "GNU General Public License, GPLv3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies [[speclj "3.0.1"]]}}
  :plugins [[speclj "3.0.1"]]
  :test-paths ["spec"])
