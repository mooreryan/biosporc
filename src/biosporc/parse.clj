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

(ns biosporc.parse
  [:use [clojure.string :only [split-lines
                               split]]])

(defn- split-lines-of-file [fname]
  (split-lines (slurp fname)))

(defn- parse-line [line]
  (let [[ref orf start stop] (split line #",")]
    [ref orf (Integer/parseInt start) (Integer/parseInt stop)
     (inc (- (Integer/parseInt stop)
             (Integer/parseInt start)))]))

(defn- zip-orf-info [orf-info]
  (zipmap [:ref :orf :start :end :len]
          (parse-line orf-info)))

(defn parse-region-file
  "Returns a map with the info from the regions file, eg
   
   [{:ref 'seq1', :orf 'orf1', :start 100, :end 300}
    {:ref 'seq1', :orf 'orf2', :start 1000, :end 1500}
    {:ref 'seq2', :orf 'orf1', :start 250, :end 750}
    {:ref 'seq2', :orf 'orf2', :start 1000, :end 1200}]" 
    [fname]
    (map zip-orf-info (split-lines-of-file fname)))

(defn region-info-into-map
  "Takes the output from parse-region-file and puts it in a nice hash
  map with keys being the references and values being the orf info
  maps."
  [region-info-maps]
  (loop [region-info-maps region-info-maps
         info {}]
    (let [this-map (first region-info-maps)]
      (if (empty? this-map) 
        info
        (let [ref-key (keyword (:ref this-map))] 
          (if (contains? info ref-key)
            (recur (rest region-info-maps)
                   (assoc info ref-key (conj (ref-key info) this-map)))
            (recur (rest region-info-maps)
                   (assoc info ref-key (vector this-map)))))))))
