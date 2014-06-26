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

(ns biosporc.core-spec
  (:require [speclj.core :refer :all]
            [biosporc.core :refer :all]))

(def base "/Users/ryanmoore/projects/wommack/biosporc/test_files/C1881")
(def sorted-bam
  (str base "/C1881.sorted.bam"))
(def bam-index
  (str base "/C1881.sorted.bam.bai"))
(def regions
  (str base "/orf_regions.csv"))

#_(describe "-main"
    (context "with proper arguments"
      (it "prints the biosporc info"
        (should-not (-main "-b" sorted-bam 
                           "-i" bam-index 
                           "-r" regions)))))
