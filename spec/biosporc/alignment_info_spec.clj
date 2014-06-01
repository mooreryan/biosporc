(ns biosporc.alignment-info-spec
  (:require [speclj.core :refer :all]
            [biosporc.alignment-info :refer :all]))

(def base "/Users/ryanmoore/projects/wommack/biosporc")
(def sorted-bam
  (str base "/test_files/unpaired.sorted.bam"))
(def bam-index
  (str base "/test_files/unpaired.sorted.bam.bai"))
(def correct-header "VN=1.5 SO:coordinate")
(def first-read 
  {:ref "seq2" :read "read1" :start 199 :end 248 :len 50})
(def last-read 
  {:ref "seq2" :read "read14" :start 1201 :end 1250 :len 50})

(describe "make-sam-reader"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (it "should have an index"
    (should (.hasIndex @sam-reader)))
  (it "returns a sam file reader of the given file"
    (should= (format "SAMFileHeader{%s}" correct-header)
             (str (.getFileHeader @sam-reader)))))

(describe "get-read-info"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with read-info (get-all-align-info @sam-reader))
  (it "gives info about all 14 reads in the test file"
    (should= 14 (count @read-info)))
  (it "returns info about the reads (first matches)"
    (should= first-read (first @read-info)))
  (it "returns info about the reads (last matches)"
    (should= last-read (last @read-info))))

(describe "get-reads"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (context "when querying contained reads"
    (it "gives the reads that are contained in interval but not overlapping"
      (should= (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}]) 
               (get-reads "seq2" 200 280 @sam-reader query-contained-reads))))
  (context "when querying overlapping reads"
    (it "gives the reads that are both contained and overlapping the interval"
      (should= (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                     {:read "read2" :ref "seq2" :start 225 :end 274 :len 50}
                     {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])
               (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads)))))

(describe "bin-reads"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with contained-reads 
        (get-reads "seq2" 200 280 @sam-reader query-contained-reads))
  (with overlapping-reads 
        (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads))
  (it "returns a map with the islanders and bridgers for a given region"
    (should= {:islanders (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
              :bridgers (set [{:read "read1" :ref "seq2" :start 199 :end 248 :len 50}
                              {:read "read3" :ref "seq2" :start 250 :end 299 :len 50}])}
             (bin-reads @contained-reads @overlapping-reads))))

