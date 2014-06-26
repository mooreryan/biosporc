;; Copyright 2014 Ryan Moore

;; This file is part of biosporc.

;; biosporc is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; biosporc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with biosporc.  If not, see
;; <http://www.gnu.org/licenses/>.

(ns biosporc.alignment-info-spec
  (:require [speclj.core :refer :all]
            [biosporc.alignment-info :refer :all]))

(def base "/Users/ryanmoore/projects/wommack/biosporc")
(def sorted-bam
  (str base "/test_files/unpaired.sorted.bam"))
(def bam-index
  (str base "/test_files/unpaired.sorted.bam.bai"))
(def correct-header "VN=1.5 SO:coordinate")
(def first-read "read1")
(def last-read "read14")

(describe "make-sam-reader"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (it "should have an index"
    (should (.hasIndex @sam-reader)))
  (it "returns a sam file reader of the given file"
    (should= (format "SAMFileHeader{%s}" correct-header)
             (str (.getFileHeader @sam-reader)))))

(describe "get-all-align-info"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (with read-info (get-all-align-info @sam-reader))
  (it "gives info about all 14 reads in the test file"
    (should= 14 (count @read-info)))
  (it "returns info about the reads (first matches)"
    (should= first-read (:read (first @read-info))))
  (it "returns info about the reads (last matches)"
    (should= last-read (:read (last @read-info)))))

(describe "get-reads"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (context "when querying contained reads"
    (it (str "gives the reads that are contained in interval but not "
             "overlapping")
      (should= ["read2"] 
               (map :read (get-reads "seq2" 200 280 @sam-reader 
                                     query-contained-reads)))))
  (context "when querying overlapping reads"
    (it (str "gives the reads that are both contained and overlapping "
             "the interval")
      (should= ["read3" "read2" "read1"] 
               (map :read (get-reads "seq2" 200 280 @sam-reader 
                                     query-overlapping-reads))))))

(describe "bin-reads"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with contained-reads 
        (get-reads "seq2" 200 280 @sam-reader query-contained-reads))
  (with overlapping-reads 
        (get-reads "seq2" 200 280 @sam-reader query-overlapping-reads))
  (with ib-map (bin-reads @contained-reads 
                          @overlapping-reads))
  (it "returns a map with the islanders and bridgers for a given region"
    (should= {:islanders ["read2"]
              :bridgers ["read3" "read1"]}
             (hash-map :islanders 
                       (map :read 
                            (:islanders @ib-map))
                       :bridgers
                       (map :read 
                            (:bridgers @ib-map))))))

(describe "single-orf-alignment-info"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with orf-map (hash-map :orf "orf1" :ref "seq2" :start 200 :end 280 
                          :len 81))
  (with ib-map (single-orf-alignment-info @orf-map @sam-reader))
  (it "takes an orf map and gives alignment info for it"
    (should= {:islanders ["read2"] :bridgers ["read3" "read1"]}
             (hash-map :islanders (map :read (:islanders @ib-map))
                       :bridgers (map :read (:bridgers @ib-map))))))

(describe "alignment-info"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with orf-maps (vector {:orf "orf1" :ref "seq2" :start 200 :end 280 
                          :len 81}
                         {:orf "orf2" :ref "seq2" :start 200 :end 280 
                          :len 81}))
  (it (str "calls single-orf-alignment info for each orf-map in given "
           "collection")
    (should= (hash-map :orf1
                       {:islanders (set [{:read "read2"
                                          :ref "seq2" 
                                          :start 225
                                          :end 274
                                          :len 50
                                          :mapped true
                                          :read-paired false
                                          :proper-pair nil
                                          :first nil :second nil
                                          :mate-mapped nil
                                          :mate-ref-name nil
                                          :inferred-insert-size nil}])
                        :bridgers (set [{:read "read1"
                                         :ref "seq2" 
                                         :start 199
                                         :end 248
                                         :len 50
                                         :mapped true
                                         :read-paired false
                                         :proper-pair nil
                                         :first nil :second nil
                                         :mate-mapped nil
                                         :mate-ref-name nil
                                         :inferred-insert-size nil}
                                        {:read "read3"
                                         :ref "seq2" 
                                         :start 250
                                         :end 299
                                         :len 50
                                         :mapped true
                                         :read-paired false
                                         :proper-pair nil
                                         :first nil :second nil
                                         :mate-mapped nil
                                         :mate-ref-name nil
                                         :inferred-insert-size nil}])}
                       :orf2
                       {:islanders (set [{:read "read2"
                                          :ref "seq2" 
                                          :start 225
                                          :end 274
                                          :len 50
                                          :mapped true
                                          :read-paired false
                                          :proper-pair nil
                                          :first nil :second nil
                                          :mate-mapped nil
                                          :mate-ref-name nil
                                          :inferred-insert-size nil}])
                        :bridgers (set [{:read "read1"
                                         :ref "seq2" 
                                         :start 199
                                         :end 248
                                         :len 50
                                         :mapped true
                                         :read-paired false
                                         :proper-pair nil
                                         :first nil :second nil
                                         :mate-mapped nil
                                         :mate-ref-name nil
                                         :inferred-insert-size nil}
                                        {:read "read3"
                                         :ref "seq2" 
                                         :start 250
                                         :end 299
                                         :len 50
                                         :mapped true
                                         :read-paired false
                                         :proper-pair nil
                                         :first nil :second nil
                                         :mate-mapped nil
                                         :mate-ref-name nil
                                         :inferred-insert-size nil}])})
             (alignment-info @orf-maps @sam-reader))))

(describe "alignment-info-for-random-orf-maps"
  (with sam-reader 
        (make-sam-reader (make-sam-reader-factory) sorted-bam bam-index))
  (with orf-map (vector {:orf "orf1" :ref "seq2" :start 200 :end 280 
                         :len 81}
                        {:orf "orf2" :ref "seq2" :start 200 :end 280 
                         :len 81}))
  (it (str "calls single-orf-alignment info for each orf-map in given "
           "collection but throw them in a vec")
    (should== (seq (vector {:islanders (set [{:read "read2"
                                              :ref "seq2" 
                                              :start 225
                                              :end 274 
                                              :len 50
                                              :mapped true
                                              :read-paired false
                                              :proper-pair nil
                                              :first nil
                                              :second nil
                                              :mate-mapped nil
                                              :mate-ref-name nil
                                              :inferred-insert-size nil}])
                            :bridgers (set [{:read "read1"
                                             :ref "seq2" 
                                             :start 199
                                             :end 248
                                             :len 50
                                             :mapped true
                                             :read-paired false
                                             :proper-pair nil
                                             :first nil
                                             :second nil
                                             :mate-mapped nil
                                             :mate-ref-name nil
                                             :inferred-insert-size nil}
                                            {:read "read3"
                                             :ref "seq2" 
                                             :start 250
                                             :end 299 
                                             :len 50
                                             :mapped true
                                             :read-paired false
                                             :proper-pair nil
                                             :first nil
                                             :second nil
                                             :mate-mapped nil
                                             :mate-ref-name nil
                                             :inferred-insert-size nil}])}
                           {:islanders (set [{:read "read2"
                                              :ref "seq2" 
                                              :start 225
                                              :end 274 
                                              :len 50
                                              :mapped true
                                              :read-paired false
                                              :proper-pair nil
                                              :first nil
                                              :second nil
                                              :mate-mapped nil
                                              :mate-ref-name nil
                                              :inferred-insert-size nil}])
                            :bridgers (set 
                                       [{:read "read1"
                                         :ref "seq2" 
                                         :start 199
                                         :end 248
                                         :len 50
                                         :mapped true
                                         :read-paired false
                                         :proper-pair nil
                                         :first nil
                                         :second nil
                                         :mate-mapped nil
                                         :mate-ref-name nil
                                         :inferred-insert-size nil}
                                        {:read "read3"
                                         :ref "seq2" 
                                         :start 250
                                         :end 299 
                                         :len 50
                                         :mapped true
                                         :read-paired false
                                         :proper-pair nil
                                         :first nil
                                         :second nil
                                         :mate-mapped nil
                                         :mate-ref-name nil
                                         :inferred-insert-size nil}])}))
              (alignment-info-for-random-orf-maps @orf-map @sam-reader))))

(describe "get-reference-lengths"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (it "gets lengths for all the references"
    (should= {:seq1 5000 :seq2 5000}
             (get-reference-lengths @sam-reader))))
