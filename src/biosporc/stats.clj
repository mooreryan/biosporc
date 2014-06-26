(ns biosporc.stats
  (:require [biosporc.alignment-info :refer :all])
  (:use [clojure.java.shell :only [sh]]
        [clojure.string :only [join replace]]))

(defn r [r-string]
  (let [{:keys [exit out err]} (apply sh ["Rscript" "-e" r-string])] 
    (if (zero? exit) out err)))

(defn wilcox 
  "Returns 1.0 if p-val is NA, else returns the p-val for R's
  wilcox.test function."  
  [ib-ratio jacknife-ib-ratios]
  (let [x (format "c(%s)" (join ", " jacknife-ib-ratios))
        mu ib-ratio
        report (r (format "wilcox.test(x=%s, mu=%s)" x mu))
        p-val-str (replace (re-find #"p-value = .*" report)
                           #"p-value = " "")]
    (if (= "NA" p-val-str) 1.0 (Double. p-val-str))))

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
                          (/ islander-count (+ islander-count 
                                               bridger-count))))))

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

(defn different-mean? [real-ib-ratio jacknife-ib-ratios]
  (let [confidence 0.05
        pval (wilcox real-ib-ratio jacknife-ib-ratios)]
    (if (<= pval confidence) pval)))

(defn different?
  "Doing 100 random orf maps cos WTFN?"
  [orf-map read-map sam-reader ref-lengths]
  (let [num 30
        ref-length ((keyword (:ref orf-map)) ref-lengths)
        
        random-orf-maps (make-random-orfs num orf-map ref-length)
        read-maps (alignment-info-for-random-orf-maps 
                   random-orf-maps sam-reader)
        ibr-ratios-from-random-orfs (ibr-ratios-for-random-orfs
                                     random-orf-maps read-maps)
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

    (different-mean? (:ib-ratio real-ib-ratio) 
                     ibr-ratios-from-random-orfs)))
