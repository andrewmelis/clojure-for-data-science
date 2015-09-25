(ns ch2-inference.data
  (:require [clojure.java.io :as io]
            [clj-time.coerce :as tc]
            [clj-time.format :as f]
            [clj-time.predicates :as p]
            [incanter.core :as i]
            [incanter.io :as iio]
            [incanter.stats :as s]))

 (defn load-data [file]
  (-> (io/resource file)
      (iio/read-dataset :header true :delim \tab)))

(defn with-parsed-data [data]
  (i/transform-col data :date (comp tc/to-local-date f/parse)))

(defn filter-weekdays [data]
  (i/$where {:date {:$fn p/weekday?}} data))

(defn mean-dwell-times-by-date [data]
  (i/$rollup :mean :dwell-time :date data))

(defn daily-mean-dwell-times [data]
  (->> (with-parsed-data data)
       (filter-weekdays)
       (mean-dwell-times-by-date)))
