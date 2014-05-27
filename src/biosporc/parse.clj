(ns biosporc.parse
  [:use [clojure.string :only [split-lines
                               split]]])

(defn- split-lines-of-file [fname]
  (split-lines (slurp fname)))

(defn- parse-line [line]
  (let [[read orf start stop] (split line #",")]
    [read orf (Integer/parseInt start) (Integer/parseInt stop)]))

(defn- zip-orf-info [orf-info]
  (zipmap [:seq :orf :start :stop]
          (parse-line orf-info)))

(defn parse-region-file
  "Returns a map with the info from the regions file, eg
   
   [{:seq 'seq1', :orf 'orf1', :start 100, :end 300}
    {:seq 'seq1', :orf 'orf2', :start 1000, :end 1500}
    {:seq 'seq2', :orf 'orf1', :start 250, :end 750}
    {:seq 'seq2', :orf 'orf2', :start 1000, :end 1200}]" 
    [fname]
    (map zip-orf-info (split-lines-of-file fname)))
