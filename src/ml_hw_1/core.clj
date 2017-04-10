(ns ml-hw-1.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

;; Constants

(def ^:const word-regex #"[A-Za-z]+")

;; Utils

(def bool-to-int #(if % 1 0))

(def join-values (partial str/join ","))
(def join-lines  (partial str/join "\n"))

(defn get-files [folder]
  (filter #(.isFile %)
    (file-seq (io/file (io/resource folder)))))

(defn get-words [file]
  (map str/lower-case
    (re-seq word-regex (slurp file))))

(defn get-files-words [files]
  (map get-words files))

(defn get-all-words [files-words]
  (apply concat files-words))

(defn calc-words-count [freqs]
  (reduce + (map second freqs)))

(defn write-to-file [file results]
  (let [lines (map join-values results)]
    (spit file (join-lines lines))))

;; Definitions

(def spam-files    (get-files "spam"))
(def ham-files     (get-files "notSpam"))
(def unknown-files (get-files "unknown"))

(def spam-files-words     (get-files-words spam-files))
(def ham-files-words      (get-files-words ham-files))
(def unknown-files-words  (get-files-words unknown-files))

(def spam-words    (get-all-words spam-files-words))
(def ham-words     (get-all-words ham-files-words))
(def unknown-words (get-all-words unknown-files-words))

;; Is spam function

(def spam-words-counts (frequencies spam-words))
(def ham-words-counts  (frequencies   ham-words))

;;; First term

(def spam-words-count (calc-words-count spam-words-counts))
(def ham-words-count  (calc-words-count ham-words-counts))

(def total-words-count (+ spam-words-count ham-words-count))

(def spam-probability (/ spam-words-count total-words-count))
(def ham-probability  (/ ham-words-count  total-words-count))

(def first-term
  (Math/log (/
              spam-probability
              ham-probability)))

;;; Second Term

(defn probability [freqs total word]
  (/ (get freqs word 0.00000001) total))

(def probability-when-spam
  (partial probability spam-words-counts spam-words-count))

(def probability-when-ham
  (partial probability ham-words-counts ham-words-count))

(defn second-term [word]
  (Math/log (/
              (probability-when-spam word)
              (probability-when-ham  word))))

;;; Determiner

(defn ratio [words]
  (reduce + first-term (map second-term words)))

(def is-spam (comp pos? ratio))

;; Main

;;; Classification

(def classified
  (map vector
    (map #(.getName %) unknown-files)
    (map (comp bool-to-int is-spam) unknown-files-words)))

;;; Top 30

(def sorted-by-spamness
  (let [all-words (distinct (concat unknown-words spam-words ham-words))]
    (map vector (sort-by second-term > all-words))))

(def top-30-spam (take      30 sorted-by-spamness))
(def top-30-ham  (take-last 30 sorted-by-spamness))

;;; Entry point

(defn -main []
  (write-to-file "./data/result.csv" classified)
  (write-to-file "./data/top.txt" (concat
                                    [["Top 30 spam:"]] top-30-spam
                                    [[]]
                                    [["Top 30 ham:"]] top-30-ham)))
