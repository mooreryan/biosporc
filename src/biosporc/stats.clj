(ns biosporc.stats
  (:require [incanter.stats :as stat]
            [biosporc.alignment-info :refer :all])
  (:import (org.apache.commons.math3.stat.inference WilcoxonSignedRankTest
                                                    TTest)))

;; (defn wilcox [jacknifed-ibrs real-ibr]
;;   (let [count (count jacknifed-ibrs)]
;;     (.wilcoxonSignedRankTest (WilcoxonSignedRankTest.) 
;;                              (double-array jacknifed-ibrs)
;;                              (double-array (repeat count real-ibr))
;;                              false)))

(defn avg-read-len 
  "The input is from the biosporc.alignment_info/bin-reads
  function--the reads from one region. Returns the mean read length of
  all the reads both islanders and bridgers together."
  [reads]
  (let [islanders (map :len (:islanders reads))
        bridgers (map :len (:bridgers reads))
        all-lengths (concat islanders bridgers)]
    (stat/mean all-lengths)))

(defn all-lengths 
  "Returns a seq of the length for each read in all reads-maps given."
  [read-maps]
  (flatten (map (fn [read-map] 
                 (concat (map :len (:islanders read-map))
                         (map :len (:bridgers read-map))))
               read-maps)))

(defn ibr 
  "Calculates the ib-ratio for one read-map. This read map represents
  the islanders and the bridgers from one region (ie one ORF)."
  [read-map]
  (let [islander-count (count (:islanders read-map))
        bridger-count (count (:bridgers read-map))]
   (hash-map :islanders islander-count
              :bridgers bridger-count
              :ib-ratio (if (zero? (+ islander-count bridger-count))
                          ;;:TODO-deal-with-this in a smarter way,
                          ;;right now since 1 is bad, make it 2 cos
                          ;;having nothing is REALLY bad
                          2
                          (/ islander-count (+ islander-count bridger-count))))))

(defn pull-orf-names [orf-map]
  (hash-map :orf (:orf orf-map)
            :ref (:ref orf-map)
            :len (:len orf-map)))

(defn ib-ratios 
  "Gives the non-normalized ib-ratios for all the orfs on a contig."
  [orf-maps read-maps]
  (map merge (map pull-orf-names orf-maps) (map ibr read-maps)))

(defn make-random-orf 
  "TODO: test that it never goes past the end of the contig"
  [base-orf contig-len]
  (let [start (rand-int (- (inc contig-len) (:len base-orf)))
        end (+ start (dec (:len base-orf)))]
    (assoc base-orf :start start :end end :orf (str "orf-" start))))

(defn make-random-orfs [n base-orf contig-len]
  (repeatedly n #(make-random-orf base-orf contig-len)))

(defn ibr-ratios-for-random-orfs [orf-maps read-maps]
  (map :ib-ratio (ib-ratios orf-maps read-maps)))

(defn different-mean? [jacknife-ib-ratios real-ib-ratio]
  (let [confidence 0.05
        pval (:p-value (stat/t-test jacknife-ib-ratios :mu real-ib-ratio))]
    (if (<= pval confidence) pval)))

(defn different?
  "Doing 30 random orf maps cos of the central limit theorem magic."
  [orf-map read-map sam-reader]
  (let [random-orf-maps (make-random-orfs 30 orf-map 5000)
        read-maps (alignment-info-for-random-orf-maps random-orf-maps sam-reader)
        ibr-ratios-from-random-orfs (ibr-ratios-for-random-orfs random-orf-maps read-maps)
        real-ib-ratio (ibr read-map)]
    
    ;; (println "this orf-map")
    ;; (clojure.pprint/pprint orf-map)
    ;; (println "this read map")
    ;; (clojure.pprint/pprint read-map)

    ;; (println "random-orf-maps")
    ;; (clojure.pprint/pprint random-orf-maps)
    ;; (println "read-maps")
    ;; (clojure.pprint/pprint read-maps)
    
    ;; (println "ibr-ratios-from-random-orfs" ibr-ratios-from-random-orfs)
    ;; (println "this ibr" (:ib-ratio real-ib-ratio))

    (different-mean? ibr-ratios-from-random-orfs (:ib-ratio real-ib-ratio))))
