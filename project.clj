(defproject biosporc "0.0.1-parallel-c"
  :description "BioSPORC"
  :url "Let's hunt some chimeras."
  :license {:name "GNU General Public License, GPLv3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [htsjdk/samtools "1.114"]
                 [picard "1.114"]
                 [org.clojure/tools.cli "0.3.1"]]
  :repositories [["local" 
                  {:url ~(str (.toURI (java.io.File. "repo")))}]]
  :profiles {:dev {:dependencies [[speclj "3.0.1"]]}}
  :plugins [[speclj "3.0.1"]]
  :test-paths ["spec"]
  :main biosporc.core)
