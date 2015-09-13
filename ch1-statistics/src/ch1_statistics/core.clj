(ns ch1-statistics.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.distributions :as d]
            [incanter.excel :as xls]
            [incanter.stats :as s]))

(defn foo
  "I don't do a whole lot."
  [x]
  (print x "Hello, World!"))

(defmulti load-data identity)

(defmethod load-data :uk [_]
  (-> (io/resource "UK2010.xls")
    (str)
    (xls/read-xls)))

(defn ex1-1 []
  (i/col-names (load-data :uk)))

(ex1-1)

(defn ex1-2 []
  (i/$ "Election Year" (load-data :uk)))

(ex1-2)

(defn ex1-3 []
  (->> (load-data :uk)
      (i/$ "Election Year")
      distinct))

(ex1-3)


(defn ex1-4 []
  (->> (load-data :uk)
      (i/$ "Election Year")
      frequencies))

(ex1-4)

(defn ex1-5 []
  (->> (load-data :uk)
       (i/$where {"Election Year" {:$ne nil}})
       (i/to-map)))

(ex1-5)

(defmethod load-data :uk-scrubbed [_]
  (->> (load-data :uk)
       (i/$where {"Election Year" {:$ne nil}})))

(defn ex1-6 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       count))

(ex1-6)

(defn mean [xs]
  (/ (reduce + xs)
     (count xs)))

(defn ex1-7 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       s/mean))

(ex1-7)

(defn median [xs]
  (let [n (count xs)
        mid (int (/ n 2))]
    (if (odd? n)
      (nth (sort xs) mid)
      (->> (sort xs)
           (drop (dec mid))
           (take 2)
           mean))))

(defn ex1-8 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       s/median))

(ex1-8)

(defn variance [xs]
  (let [x-bar (s/mean xs)
        n (count xs)
        square-deviation (fn [x]
                           (i/sq (- x x-bar)))]
    (mean (map square-deviation xs))))

(defn standard-deviation [xs]
  (i/sqrt (variance xs)))

(defn ex1-9 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       standard-deviation))

(ex1-9)

(defn quantile [q xs]
  (let [n (dec (count xs))
        i (-> (* n q)
              (+ 1/2)
              int)]
    (nth (sort xs) i)))

(defn ex1-10 []
  (let [xs (->> (load-data :uk-scrubbed)
                (i/$ "Electorate"))]
    (map #(quantile % xs) [0 1/4 1/2 3/4 1])))

(ex1-10)

(defn ex1-10-with-incanter []
  (let [xs (->> (load-data :uk-scrubbed)
                (i/$ "Electorate"))]
    (s/quantile xs :probs [0 1/4 1/2 3/4 1])))

(ex1-10-with-incanter)

(defn bin [n-bins xs]
  (let [min-x (apply min xs)
        max-x (apply max xs)
        range-x (- max-x min-x)
        bin-fn (fn [x]
                 (-> x
                     (- min-x)
                     (/ range-x)
                     (* n-bins)
                     int
                     (min (dec n-bins))))]
    (map bin-fn xs)))

(defn ex1-11 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       (bin 5)
       frequencies))

(ex1-11)

(defn ex1-12 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       (c/histogram)
       (i/view)))

(defn ex1-12-save []
  (i/save (->> (load-data :uk-scrubbed)
               (i/$ "Electorate")
               (c/histogram))
          "resources/secondtry.png")) ; can prefix with a dir

(ex1-12)
(ex1-12-save)

(defn uk-electorate []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")))

(defn ex1-13 []
  (-> (uk-electorate)
      (c/histogram :nbins 200)
      (i/view)))

(ex1-13)

(defn ex1-14 []
  (-> (i/$ "Electorate" (load-data :uk-scrubbed))
      (c/histogram :x-label "UK electorate"
                   :nbins 20)
      (i/view)))

(ex1-14)

(defn ex1-15 []
  (let [xs (take 10000 (repeatedly rand))]
    (i/view (c/histogram xs
                         :x-label "uniform distribution"
                         :nbins 20))))

(ex1-15)

(defn ex1-16 []
  (let [xs (->> (repeatedly rand)
                (partition 10)
                (map mean)
                (take 10000))]
    (i/view (c/histogram xs
                         :x-label "distribution of means"
                         :nbins 20))))

(ex1-16)

(defn ex1-17 []
  (let [distribution (d/normal-distribution)
        xs (take 10000 (repeatedly #(d/draw distribution)))]
    (i/view (c/histogram xs
                         :x-label "normal distribution"
                         :nbins 20))))
    
(ex1-17)

(defn honest-baker [mean sd]
  (let [distribution (d/normal-distribution mean sd)]
    (repeatedly #(d/draw distribution))))

(defn ex1-18 []
  (-> (take 10000 (honest-baker 100 30))
      (c/histogram :x-label "honest baker"
                   :nbins 25)
      (i/view)))

(ex1-18)

(defn dishonest-baker [mean sd]
  (let [distribution (d/normal-distribution mean sd)]
    (->> (repeatedly #(d/draw distribution))
         (partition 13)
         (map (partial apply max)))))

(defn ex1-19 []
  (-> (take 10000 (dishonest-baker 950 30))
      (c/histogram :x-label "dishonest baker"
                   :nbins 25)
      (i/view)))

(ex1-19)

(defn ex1-20 []
  (let [weights (take 10000 (dishonest-baker 950 30))]
    {:mean (s/mean weights)
     :median (s/median weights)
     :skewness (s/skewness weights)}))

(ex1-20)

(defn ex1-21 []
  (->> (honest-baker 1000 30)
       (take 10000)
       (c/qq-plot)
       (i/view))
  (->> (dishonest-baker 1000 30)
       (take 10000)
       (c/qq-plot)
       (i/view)))

(ex1-21)

(defn ex1-22 []
  (-> (c/box-plot (->> (honest-baker 1000 30)
                       (take 10000))
                  :legend true
                  :y-label "loaf weight (g)"
                  :series-label "honest baker")
      (c/add-box-plot (->> (dishonest-baker 950 30)
                           (take 10000))
                      :series-label "dishonest baker")
      (i/view)))

(ex1-22)

(defn ex1-23 []
  (let [sample-honest (->> (honest-baker 1000 30)
                           (take 1000))
        sample-dishonest (->> (dishonest-baker 950 30)
                              (take 1000))
        ecdf-honest (s/cdf-empirical sample-honest)
        ecdf-dishonest (s/cdf-empirical sample-dishonest)]
    (-> (c/xy-plot sample-honest (map ecdf-honest sample-honest)
                   :x-label "loaf weight"
                   :y-label "probability"
                   :legend true
                   :series-label "honest baker")
        (c/add-lines sample-dishonest (map ecdf-dishonest sample-dishonest)
                     :series-label "dishonest baker")
        (i/view))))

(ex1-23)

(defn ex1-24 []
  (let [electorate (->> (load-data :uk-scrubbed)
                        (i/$ "Electorate"))
        ecdf (s/cdf-empirical electorate)
        fitted (s/cdf-normal electorate
                             :mean (s/mean electorate)
                             :sd (s/sd electorate))]
    (-> (c/xy-plot electorate fitted
                   :x-label "Electorate"
                   :y-label "Probability"
                   :series-label "Fitted"
                   :legend true)
        (c/add-lines electorate (map ecdf electorate)
                     :series-lavel "Empirical")
        (i/view))))

(ex1-24)

(defn ex1-25 []
  (->> (load-data :uk-scrubbed)
       (i/$ "Electorate")
       (c/qq-plot)
       (i/view)))

(ex1-25)

(defn ex1-26 []
  (->> (load-data :uk-scrubbed)
       (i/add-derived-column :victors [:Con :LD] +)))

;; (ex1-26)

(->> (load-data :uk-scrubbed)
     (i/$ "LD")
     (map type)
     frequencies)

(defn ex1-27 []
  (->> (load-data :uk-scrubbed)
       (i/$where #(not-any? number? [(% "Con") (% "LD")]))
       (i/$ [:Region :Electorate :Con :LD])))

(ex1-27)

(defmethod load-data :uk-victors [_]
  (->> (load-data :uk-scrubbed)
       (i/$where {:Con {:$fn number?} :LD {:$fn number?}})
       (i/add-derived-column :victors [:Con :LD] +)
       (i/add-derived-column :victors-share [:victors :Votes] /)
       (i/add-derived-column :turnout [:Votes :Electorate] /)))

(load-data :uk-victors)

(defn ex1-28 []
  (->> (load-data :uk-victors)
       (i/$ :victors-share)
       (c/qq-plot)
       (i/view)))

(ex1-28)

(defmethod load-data :ru [_]
  (i/conj-rows (-> (io/resource "Russia2011_1of2.xls")
                   str
                   (xls/read-xls))
               (-> (io/resource "Russia2011_2of2.xls")
                   str
                   (xls/read-xls))))

(defn ex1-29 []
  (-> (load-data :ru)
      (i/col-names)))

(ex1-29)

(defmethod load-data :ru-victors [_]
  (->> (load-data :ru)
       (i/rename-cols
        {"Number of voters included in voters list" :electorate
         "Number of valid ballots" :valid-ballots
         "United Russia" :victors})
       (i/add-derived-column :victors-share
                             [:victors :valid-ballots] i/safe-div)
       (i/add-derived-column :turnout
                             [:valid-ballots :electorate] /)))

(defn ex1-30 []
  (-> (i/$ :turnout (load-data :ru-victors))
      (c/histogram :x-label "Russia turnout"
                   :nbins 20)
      (i/view)))
              
(ex1-30)

(defn ex1-31 []
  (->> (load-data :ru-victors)
       (i/$ :turnout)
       c/qq-plot
       i/view))

(ex1-31)

(defn as-pmf [bins]
  (let [histogram (frequencies bins)
        total (reduce + (vals histogram))]
    (->> histogram
         (map (fn [[k v]]
                [k (/ v total)]))
         (into {}))))

(defn ex1-32 []
  (let [n-bins 40
        uk (->> (load-data :uk-victors)
                (i/$ :turnout)
                (bin n-bins)
                as-pmf)
        ru (->> (load-data :ru-victors)
                (i/$ :turnout)
                (bin n-bins)
                as-pmf)]
    (-> (c/xy-plot (keys uk) (vals uk)
                   :series-label "UK"
                   :legend true
                   :x-label "turnout bins"
                   :y-label "probability")
        (c/add-lines (keys ru) (vals ru)
                     :series-label "Russia")
        (i/view))))

(ex1-32)

(defn ex1-33 []
  (let [data (load-data :uk-victors)]
    (-> (c/scatter-plot (i/$ :turnout data)
                        (i/$ :victors-share data)
                        :x-label "turnout"
                        :y-label "victor's share")
        (i/view))))

(ex1-33)

(defn ex1-34 []
  (let [data (load-data :ru-victors)]
    (-> (c/scatter-plot (i/$ :turnout data)
                        (i/$ :victors-share data)
                        :x-label "turnout"
                        :y-label "victor's share")
        (i/view))))

(ex1-34)

(defn ex1-35 []
  (let [data (-> (load-data :ru-victors)
                 (s/sample :size 10000))]
    (-> (c/scatter-plot (i/$ :turnout data)
                        (i/$ :victors-share data)
                        :x-label "turnout"
                        :y-label "victor's share")
        (c/set-alpha 0.05)
        (i/view))))

(ex1-35)
