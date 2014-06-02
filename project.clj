(defproject biosporc_new "0.1.0-SNAPSHOT"
  :description "New version of BioSPORC"
  :url "Let's hunt some chimeras."
  :license {:name "GNU General Public License, GPLv3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [me.raynes/fs "1.4.4"]
                 [htsjdk/samtools "1.114"]
                 [picard "1.114"]
                 [incanter "1.5.5"]
                 [org.apache.commons/commons-math3 "3.3"]]
  :repositories [["local" 
                  {:url ~(str (.toURI (java.io.File. "repo")))}]]
  :profiles {:dev {:dependencies [[speclj "3.0.1"]]}}
  :plugins [[speclj "3.0.1"]]
  :test-paths ["spec"])
