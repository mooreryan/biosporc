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
  (let [sr-factory (make-sam-reader-factory)
        sam-reader (make-sam-reader sr-factory sorted-bam bam-index)]
    (it "should have an index"
      (should (.hasIndex sam-reader)))
    (it "returns a sam file reader of the given file"
      (should= (format "SAMFileHeader{%s}" correct-header)
               (str (.getFileHeader sam-reader))))))

(describe "get-read-info"
  (let [sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam 
                                    bam-index)
        read-info (get-align-info sam-reader)]
    (it "gives info about all 14 reads in the test file"
      (should= 14 (count read-info)))
    (it "returns info about the reads (first matches)"
      (should= first-read (first read-info)))
    (it "returns info about the reads (last matches)"
      (should= last-read (last read-info)))))
