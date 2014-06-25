(ns biosporc.stats-spec
  (:require [speclj.core :refer :all]
            [biosporc.stats :refer :all]
            [biosporc.alignment-info :refer :all]
            [incanter.stats :as stat]))

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

(describe "avg-read-len"
  (with reads 
        (hash-map :islanders
                  (set [{:read "read2" :ref "seq2" :start 225 :end 274 
                         :len 50}])
                  :bridgers
                  (set [{:read "read3" :ref "seq2" :start 301 :end 400 
                         :len 100}])))
  (it "gets mean of all reads both islanders and bridgers for one region"
    (should= 75.0
             (avg-read-len @reads))))

(defn build-orf [start len]
  (hash-map :orf (str "orf-" start) 
            :ref "seq2" 
            :start start 
            :end (dec (+ start len)) 
            :len len))

(defn build-read [start]
  (hash-map :read (str "read-" start) 
            :ref "seq2" 
            :start start 
            :end (dec (+ start 100)) 
            :len 100))

(def a-contigs-reads
  [{:islanders (set (map build-read (range 101 201 10))) ;; 10
    :bridgers (set (map build-read [;; overlapping the start
                                    51 61 71 
                                    ;; overlapping the ends
                                    551 561 571]))} ;; 6
   {:islanders (set (map build-read (range 601 721 10))) ;; 12
    :bridgers (set (map build-read [;; overlap the start
                                    551 561 571 
                                    ;; overlap the end
                                    951 961]))} ;; 5
   {:islanders (set (map build-read (range 1201 1281 10))) ;; 8
    :bridgers (set (map build-read [;; overlap the start
                                    951 961
                                    ;; over the end
                                    1451 1461 1471 1481 1491]))}]) ;; 7

(describe "all-lengths"
  (it "gets all the lengths from a collection of read maps"
    (should= (repeat 48 100)
             (all-lengths a-contigs-reads))))

(describe "ibr"
  (with read-map 
        {:islanders (set (map build-read (range 101 201 10)))
         :bridgers (set (map build-read [51 61 71 551 561 571]))})
  (with no-is-or-bs {:islanders #{} :bridgers #{}})

  (context "as long as there is at least one islander or bridger"
    (it "gets the irb for a read map from one region"
      (should= {:islanders 10 :bridgers 6 :ib-ratio 10/16}
               (ibr @read-map))))
  (context "if there are no islanders or bridgers"
    (it "returns an ib-ratio of 2"
      (should= {:islanders 0, :bridgers 0, :ib-ratio 2}
               (ibr @no-is-or-bs)))))

(describe "ib-ratios"
  (with a-contigs-orfs 
        (let [starts [101 601 1201]
              lengths [500 400 300]]
          (map build-orf starts lengths)))
  (with a-contigs-reads 
        [{:islanders (set (map build-read (range 101 201 10))) ;; 10
          :bridgers (set (map build-read [;; overlapping the start
                                          51 61 71 
                                          ;; overlapping the ends
                                          551 561 571]))} ;; 6
         {:islanders (set (map build-read (range 601 721 10))) ;; 12
          :bridgers (set (map build-read [;; overlap the start
                                          551 561 571 
                                          ;; overlap the end
                                          951 961]))} ;; 5
         {:islanders (set (map build-read (range 1201 1281 10))) ;; 8
          :bridgers (set (map build-read [;; overlap the start
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
  (with read-map (single-orf-alignment-info 
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
                             @read-map 
                             @sam-reader 
                             @ref-lengths)))))

  (with good-orf {:orf "orf1" :ref "C4169" :start 2 :end 163 
                  :len (inc (- 163 2))})
  (with good-sr (make-sam-reader (make-sam-reader-factory) 
                                 c4169-bam
                                 c4169-bai))
  (with good-read-map (single-orf-alignment-info @good-orf @good-sr))
  (with good-ref-lengths (get-reference-lengths @good-sr))

  (context (str "when the mean of the jacknifes is not different from "
                "the real ib-ratio")
    (it "returns nil"
      (pending "Need to make a contig with a good ratio.")
      (should-not (different? @good-orf
                              @good-read-map 
                              @good-sr
                              @good-ref-lengths)))))
