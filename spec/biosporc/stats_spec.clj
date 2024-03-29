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

(ns biosporc.stats-spec
  (:require [speclj.core :refer :all]
            [biosporc.stats :refer :all]
            [biosporc.alignment-info :refer :all]))

(def base "/Users/ryanmoore/projects/wommack/biosporc/test_files")
(def sorted-bam
  (str base "/unpaired.sorted.bam"))
(def bam-index
  (str base "/unpaired.sorted.bam.bai"))

(def c4169-bam (str base "/C4169/C4169.sorted.bam"))
(def c4169-bai (str base "/C4169/C4169.sorted.bam.bai"))

(describe "r"
  (with working-script "sum <- 2 + 2;cat(sum)")
  (with broken-script "sum <- 2 + 2;cat(sum))")
  (context "the R script terminates with exit code 0"
    (it "returns the standard out as a string"
      (should= "4"
               (r @working-script))))
  (context "the R script terminates with failures"
    (it "returns the stderr output"
      (should= (str "Error: unexpected ')' in \"cat(sum))\"\n"
                    "Execution halted\n")
               (r @broken-script)))))

(describe "wilcox"
  (with jacknife-ib-ratios [1/2 1/3 1/2 1/4 2/3 1/6 4/5])
  (with real-ib-ratio 1/3)
  (it "returns the p-value from the non-paired wilcox test"
    (should= 0.1411
             (wilcox @real-ib-ratio @jacknife-ib-ratios)))
  (context "with p-val of NA"
    (it "returns a value of 1"
      (should= 1.0
               (wilcox 1 [1 1 1 1 1])))))

(defn build-orf [start len]
  (hash-map :orf (str "orf-" start) 
            :ref "seq2" 
            :start start 
            :end (dec (+ start len)) 
            :len len))

(defn build-read [mapped start]
  (hash-map :read (str "read-" start) 
            :ref "seq2" 
            :start start 
            :end (dec (+ start 100)) 
            :len 100
            :mapped mapped))

(def a-contigs-reads
  [{:islanders (set (map #(build-read true %) (range 101 201 10))) ;; 10
    :bridgers (set (map #(build-read true %) [;; overlapping the start
                                              51 61 71 
                                              ;; overlapping the ends
                                              551 561 571]))} ;; 6
   {:islanders (set (map #(build-read true %) (range 601 721 10))) ;; 12
    :bridgers (set (map #(build-read true %) [;; overlap the start
                                              551 561 571 
                                              ;; overlap the end
                                              951 961]))} ;; 5
   {:islanders (set (map #(build-read true %) (range 1201 1281 10))) ;; 8
    :bridgers (set (map #(build-read true %) 
                        [;; overlap the start
                         951 961
                         ;; over the end
                         1451 1461 1471 1481 1491]))}]) ;; 7

(describe "filter-unmapped-reads"
  (it "filters unmapped reads from coll of read maps"
    (should= [{:ref "seq2" :read "read2" :mapped true}]
             (filter-unmapped-reads [{:ref "seq2" :read "read1" 
                                      :mapped false}
                                     {:ref "seq2" :read "read2" 
                                      :mapped true}]))))

(describe "collapse-proper-pairs"
  (it "keeps only the first read of any proper pair"
    (should= [{:ref "seq2" :read "read1" :mapped true :proper-pair true
               :first true}]
             (collapse-proper-pairs 
              [{:ref "seq2" :read "read1" :mapped true :proper-pair true
                :first true}
               {:ref "seq2" :read "read2" :mapped true :proper-pair true
                :first false}
               {:ref "seq2" :read "read2" :mapped true :proper-pair false
                :first nil}]))))

(describe "keep-mapped-singles-and-collapse-proper-pairs"
  (it (str "keeps only the first read of any proper pair or a read not "
           "in a proper pair if it is mapped")
    (should= [{:ref "seq2" :read "read1" :mapped true :proper-pair true
               :first true}
              {:ref "seq2" :read "read2" :mapped true :proper-pair false
                :first nil}]
             (keep-mapped-singles-and-collapse-proper-pairs
              [{:ref "seq2" :read "read1" :mapped true :proper-pair true
                :first true}
               {:ref "seq2" :read "read2" :mapped true :proper-pair true
                :first false}
               {:ref "seq2" :read "read2" :mapped true :proper-pair false
                :first nil}]))))

(describe "ibr"
  (with read-map 
        {:islanders (set (map #(build-read true %) (range 101 201 10)))
         :bridgers (set (map #(build-read true %) 
                             [51 61 71 551 561 571]))})
  (with no-is-or-bs {:islanders #{} :bridgers #{}})

  (context "as long as there is at least one islander or bridger"
    (it "gets the irb for a read map from one region"
      (should= {:islanders 10 :bridgers 6 :ib-ratio 10/16}
               (ibr @read-map))))
  (context "if there are no islanders or bridgers"
    (it "returns an ib-ratio of 2"
      (should= {:islanders 0, :bridgers 0, :ib-ratio 2}
               (ibr @no-is-or-bs))))
  (context "with bad reads"
    (it "will throw out unmapped and second reads of proper pairs"
      (pending "make a test that checks this"))))

(describe "ib-ratios"
  (with a-contigs-orfs 
        (let [starts [101 601 1201]
              lengths [500 400 300]]
          (map build-orf starts lengths)))
  (with a-contigs-reads 
        [{:islanders (set (map #(build-read true %) (range 101 201 10))) ;; 10
          :bridgers (set (map #(build-read true %) [;; overlapping the start
                                                    51 61 71 
                                                    ;; overlapping the ends
                                                    551 561 571]))} ;; 6
         {:islanders (set (map #(build-read true %) (range 601 721 10))) ;; 12
          :bridgers (set (map #(build-read true %) [;; overlap the start
                                                    551 561 571 
                                                    ;; overlap the end
                                                    951 961]))} ;; 5
         {:islanders (set (map #(build-read true %) (range 1201 1281 10))) ;; 8
          :bridgers (set (map #(build-read true %) [;; overlap the start
                                                    951 961
                                                    ;; over the end
                                                    1451 1461 1471 1481 1491]))}]) ;; 7
  (it "gives the ib-ratios for each orf"
    (should= 
     [{:orf "orf-101" :ref "seq2" :len 500 :islanders 10 :bridgers 6 
       :ib-ratio 10/16}
      {:orf "orf-601" :ref "seq2" :len 400 :islanders 12 :bridgers 5 
       :ib-ratio 12/17}
      {:orf "orf-1201" :ref "seq2" :len 300 :islanders 8 :bridgers 7 
       :ib-ratio 8/15}]
     (ib-ratios @a-contigs-orfs @a-contigs-reads))))


(describe "make-random-orf"
  (with start 150)
  (with length 300)
  (with base-orf (build-orf @start @length))
  (it (str "makes an orf like the given one, but starting at a "
           "different location")
    (let [random-orf (make-random-orf @base-orf 1000)] 
      (should= (build-orf (:start random-orf) @length)
               random-orf))))

(describe "make-random-orfs"
  (with start 150)
  (with length 300)
  (it "calls make-random-orf as many times as you ask for"
    (should= 10
             (count (make-random-orfs 10 (build-orf @start @length) 
                                      1000)))))

(describe "ibr-ratios-for-random-orfs"
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam bam-index))
  (with random-orf-maps (make-random-orfs 30 (build-orf 150 300) 1000))
  (with read-maps (alignment-info-for-random-orf-maps @random-orf-maps 
                                                      @sam-reader))
  
  (it (str "returns a seq of the ibr ratios for each orf represented "
           "in the given orf map")
    (should= 30
             (count (ibr-ratios-for-random-orfs @random-orf-maps
                                                @read-maps)))))

(describe "different-mean?"
  (with jacknife-ib-ratios [1/2 1/3 1/2 1/4 2/3 1/6 4/5])
  (with sig-jacknife-ib-ratios [1/2 1/2 1/2 1/4 2/3 1/2 4/5])
  (with real-ib-ratio 1/3)

  (context "with something that is not significant"
    (it "returns nil"
      (should= nil
               (different-mean? @real-ib-ratio @jacknife-ib-ratios))))
  (context "with something that is significantly different"
    (it "returns the p-value"
      ;; test value is from R stats software
      (should= 0.03142
               (different-mean? @real-ib-ratio 
                                @sig-jacknife-ib-ratios)))))

;; I want a function that given an orf map, it performs the jacknifing
;; and then checks to see if the mean of the ib-ratios of the
;; jacknified orfs is different from the ib-ratio of the real orf. If
;; it is different, the ORF is bad and you should worry about it, but
;; if it isn't different, the ORF is good. Hooray!
(describe "different?"
  (with orf-map (build-orf 100 1000))
  (with single-orf-ibr-info (single-orf-alignment-info 
                             @orf-map @sam-reader))
  (with sam-reader (make-sam-reader (make-sam-reader-factory) 
                                    sorted-bam 
                                    bam-index))
  (with ref-lengths (get-reference-lengths @sam-reader))
  (with confidence 0.5)

  

  (context (str "when the mean of the jacknifes is different from the "
                "real ib-ratio")
    (it "gives the p-val"
      (should (> @confidence
                 (different? @orf-map 
                             @single-orf-ibr-info 
                             @sam-reader 
                             @ref-lengths)))))

  (with good-orf {:orf "orf1" :ref "C4169" :start 2 :end 163 
                  :len (inc (- 163 2))})
  (with good-sr (make-sam-reader (make-sam-reader-factory) 
                                 c4169-bam
                                 c4169-bai))
  (with good-single-orf-ibr-info (single-orf-alignment-info @good-orf 
                                                            @good-sr))
  (with good-ref-lengths (get-reference-lengths @good-sr))

  (context (str "when the mean of the jacknifes is not different from "
                "the real ib-ratio")
    (it "returns nil"
      (pending "Need to make a contig with a good ratio.")
      (should-not (different? @good-orf
                              @good-single-orf-ibr-info 
                              @good-sr
                              @good-ref-lengths)))))
