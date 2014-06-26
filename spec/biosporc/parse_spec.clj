(ns biosporc.parse-spec
  (:require [speclj.core :refer :all]
            [biosporc.parse :refer :all]))

(def region-file (str "/Users/ryanmoore/projects/wommack/biosporc/"
                      "test_files/region-file.csv"))

(describe "parse-region-file"
  (it "returns a map with info from regions file"
    (let [region-map [{:ref "seq1", :orf "orf1", :start 100, :end 300
                       :len 201}
                      {:ref "seq1", :orf "orf2", :start 1000, :end 1500
                       :len 501}
                      {:ref "seq2", :orf "orf1", :start 250, :end 750,
                       :len 501}
                      {:ref "seq2", :orf "orf2", :start 770, :end 1200
                       :len (inc (- 1200 770))}]]
      (should= region-map
               (parse-region-file region-file)))))

(describe "region-info-into-map"
  (it "parses the region info into one hash-map"
    (should= (hash-map 
              :seq1 
              [{:ref "seq1", :orf "orf1", :start 100, :end 300 :len 201}
               {:ref "seq1", :orf "orf2", :start 1000, :end 1500 
                :len 501}]
              :seq2
              [{:ref "seq2", :orf "orf1", :start 250, :end 750
                :len 501}
               {:ref "seq2", :orf "orf2", :start 770, :end 1200
                :len (inc (- 1200 770))}])
             (region-info-into-map (parse-region-file region-file)))))
