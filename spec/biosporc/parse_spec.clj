(ns biosporc.parse-spec
  (:require [speclj.core :refer :all]
            [biosporc.parse :refer :all]))


(describe "parse-region-file"
  (it "returns a map with info from regions file"
    (let [region-file (str "/Users/ryanmoore/projects/wommack/biosporc/"
                           "test_files/region-file.csv")
          region-map [{:seq "seq1", :orf "orf1", :start 100, :stop 300}
                      {:seq "seq1", :orf "orf2", :start 1000, :stop 1500}
                      {:seq "seq2", :orf "orf1", :start 250, :stop 750}
                      {:seq "seq2", :orf "orf2", :start 770, :stop 1200}]]
      (should= region-map
               (parse-region-file region-file)))))
