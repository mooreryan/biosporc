(ns biosporc.stats-spec
  (:require [speclj.core :refer :all]
            [biosporc.stats :refer :all]))

(describe "avg-read-len"
  (with reads 
        (hash-map :islanders
                  (set [{:read "read2" :ref "seq2" :start 225 :end 274 :len 50}])
                  :bridgers
                  (set [{:read "read3" :ref "seq2" :start 301 :end 400 :len 100}])))
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
  (it "gets the irb for a read map from one region"
    (should= {:islanders 10 :bridgers 6 :ib-ratio 10/16}
             (ibr @read-map))))

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
    (should= [{:orf "orf-101" :ref "seq2" :islanders 10 :bridgers 6 :ib-ratio 10/16}
              {:orf "orf-601" :ref "seq2" :islanders 12 :bridgers 5 :ib-ratio 12/17}
              {:orf "orf-1201" :ref "seq2" :islanders 8 :bridgers 7 :ib-ratio 8/15}]
             (ib-ratios @a-contigs-orfs @a-contigs-reads))))
