(ns biosporc.alignment-info
  (:require [clojure.java.io :as io])
  (:import (htsjdk.samtools SamReaderFactory
                            SamInputResource
                            ValidationStringency)))

(defn make-sam-reader-factory []
  (.validationStringency (SamReaderFactory/makeDefault)
                         (ValidationStringency/valueOf "LENIENT")))

(defn make-sam-reader 
  "TODO: Doesn't check if files exist.
   TODO: .hasIndex() always returns true"
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

(defn get-align-info [sam-reader]
  (let [iter (lazy-seq (iterator-seq (.iterator sam-reader)))]
    (map get-record-info iter)))
