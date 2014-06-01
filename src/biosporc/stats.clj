(ns biosporc.stats
  (:require [incanter.stats :as stat]))

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
              :ib-ratio (/ islander-count (+ islander-count bridger-count)))))

(defn pull-orf-names [orf-map]
  (hash-map :orf (:orf orf-map)
            :ref (:ref orf-map)))

(defn ib-ratios 
  "Gives the non-normalized ib-ratios for all the orfs on a contig."
  [orf-maps read-maps]
  (map merge (map pull-orf-names orf-maps) (map ibr read-maps)))
