(ns biosporc.alignment-info
  (:require [clojure.java.io :as io])
  (:import (htsjdk.samtools SamReaderFactory
                            SamInputResource
                            ValidationStringency)))

(defn make-sam-reader-factory []
  (.validationStringency (SamReaderFactory/makeDefault)
                         (ValidationStringency/valueOf "SILENT")))

(defn make-sam-reader 
  "TODO: Doesn't handle file exceptions"
  [sr-fac sorted-bam bam-index]
  (.open sr-fac
         (.index (SamInputResource/of (io/file sorted-bam))
                 (io/file bam-index))))

(defn get-length [start end]
  (inc (- end start)))

(defn get-record-info [bam-record]
  (let [start (.getAlignmentStart bam-record)
        end (.getAlignmentEnd bam-record)]
    (hash-map :ref (.getReferenceName bam-record)
              :read (.getReadName bam-record)
              :start start
              :end end
              :len (get-length start end))))

(defn get-all-align-info
  "Returns a seq of maps containing info for all sequences."
  [sam-reader]
  (let [iter (lazy-seq (iterator-seq (.iterator sam-reader)))]
    (map get-record-info iter)))

;; TODO consider laziness by using filters and lazy-seq around the
;; iterator-seq instead of set differences

(defn query-contained-reads [seq start end sam-reader]
  (.queryContained sam-reader seq start end))

(defn query-overlapping-reads [seq start end sam-reader]
  (.queryOverlapping sam-reader seq start end))

(defn get-reads 
  "Gets reads that are either contained or overlapping the given
  interval depending on whehter query-contained-reads or
  query-overlapping-reads is passed. Closes the iterator."  
  [seq start end sam-reader query-fn]
  (let [iter (query-fn seq start end sam-reader)
        reads (iterator-seq iter)
        read-set (set (map get-record-info reads))]
    (.close iter)
    read-set))

(defn bin-reads 
  "Partitions the contained and overlapping reads into islanders and
  bridgers."
  [contained-reads overlapping-reads]
  (hash-map :islanders contained-reads
            :bridgers (clojure.set/difference overlapping-reads contained-reads)))
