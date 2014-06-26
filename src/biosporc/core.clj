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

(ns biosporc.core
  (:require [biosporc.alignment-info :refer :all]
            [biosporc.parse :refer :all]
            [biosporc.stats :refer :all]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class :main true))

(defn exist? [fname]
  (.exists (clojure.java.io/file fname)))

(def cli-options
  [["-b" "--sorted-bam RECRUITMENT.bam" "A sorted BAM file"
    :validate [exist? "The bam file doesn't exist!"]]
   ["-i" "--bam-index RECRUITMENT.bam.bai" "A BAM index file"
    :validate [exist? "The bam index doesn't exist!"]]
   ["-r" "--region-file regions.csv" "A CSV with region info"
    :validate [exist? "The region file doesn't exist!"]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (clojure.string/join 
   \newline 
   ["" ""
    "SYNOPSIS:"
    "Does the biosporc stuff; doesn't take into account pairing."
    ""
    (str "USAGE: \njava -jar biosporc-x.y.z.jar "
         "-b <bam-file> -i <index-file> -r <regions-file>")
    ""
    "Options:"
    options-summary
    ""]))

(defn error-msg [errors]
  (str "\nERROR:\n" (clojure.string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn- print-info [info-map]
  (println (format "%s\t%s\t%s" 
                   (name (:ref info-map)) 
                   (name (:orf info-map)) 
                   (:flag info-map))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-options)]
    ;; help and error conditions
    (cond
     (:help options) (exit 0 (usage summary))
     errors (exit 1 (error-msg errors))
     (not= (count options) 3) (exit 1 (str "Incorrect number of options!"
                                           (usage summary))))
    
    ;; run program with options
    (let [each-refs-orf-maps (region-info-into-map 
                              (parse-region-file (:region-file options)))
          refs (keys each-refs-orf-maps)
          sam-reader (make-sam-reader (make-sam-reader-factory)
                                      (:sorted-bam options)
                                      (:bam-index options))
          ref-lengths (get-reference-lengths sam-reader)
          biosporc-maps 
          (zipmap refs 
                  (map (fn [rec] 
                         (let [reference (first rec) orf-maps (last rec)]
                           (alignment-info orf-maps sam-reader)))
                       each-refs-orf-maps))]
      (println "ref\torf\tflag")
      (doall (map print-info
                  (flatten (map (fn call-different?-for-each-reference [ref]
                                  (let [orf-maps (ref each-refs-orf-maps)
                                        biosporc-map (ref biosporc-maps)]
                                    (map (fn call-different? [x y] 
                                           (let [orf-map x
                                                 [orf ibr-info] y]
                                             (hash-map :ref ref
                                                       :orf orf
                                                       :flag
                                                       (different? orf-map 
                                                                   ibr-info 
                                                                   sam-reader 
                                                                   ref-lengths))))
                                         orf-maps biosporc-map))) 
                                refs)))))))
