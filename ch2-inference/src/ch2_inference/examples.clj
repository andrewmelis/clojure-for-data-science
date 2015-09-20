(ns ch2-inference.examples
  (:require [clojure.java.io :as io]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.io :as iio]))

(defn load-data [file]
  (-> (io/resource file)
      (iio/read-dataset :header true :delim \tab)))

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
