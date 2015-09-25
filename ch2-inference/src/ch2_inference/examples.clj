(ns ch2-inference.examples
  (:require [ch2-inference.data :refer :all]
            [clojure.java.io :as io]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.io :as iio]
            [incanter.stats :as s]))

(defn ex-2-1 []
  (-> (load-data "dwell-times.tsv")
      (i/view)))

(defn ex-2-2 []
  (-> (i/$ :dwell-time (load-data "dwell-times.tsv"))
      (c/histogram :x-label "Dwell time (s)"
                   :nbins 50)
      (i/view)))

(defn ex-2-3 []
  (-> (i/$ :dwell-time (load-data "dwell-times.tsv"))
      (c/histogram :x-label "Dwell time (s)"
                   :nbins 20)
      (c/set-axis :y (c/log-axis :label "Log Frequency"))
      (i/view)))

(defn ex-2-4 []
  (let [dwell-times (->> (load-data "dwell-times.tsv")
                         (i/$ :dwell-time))]
    (println "Mean: " (s/mean dwell-times))
    (println "Median: " (s/median dwell-times))
    (println "SD: " (s/sd dwell-times))))

(defn ex-2-5 []
  (let [means (->> (load-data "dwell-times.tsv")
                   (daily-mean-dwell-times)
                   (i/$ :dwell-time))]
    (println "Mean: " (s/mean means))
    (println "Median: " (s/median means))
    (println "SD: " (s/sd means))))

(defn ex-2-6 []
  (let [means (->> (load-data "dwell-times.tsv")
                   (daily-mean-dwell-times)
                   (i/$ :dwell-time))]
    (-> (c/histogram means
                     :x-label "Daily mean dwell time (s)"
                     :nbins 20))))
