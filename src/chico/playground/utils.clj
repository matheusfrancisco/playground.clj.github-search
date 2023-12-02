(ns chico.playground.utils
  (:require
   [clojure.test :refer [deftest is testing]])
  (:import
   (java.time LocalDate ZoneOffset)
   (java.time.format DateTimeFormatter)
   (java.time.temporal ChronoUnit)))

(defn get-dates []
  (let [today (LocalDate/now)
        start-date (.minus (LocalDate/now) 2 (ChronoUnit/YEARS))]
    (take-while (fn [date] (not (.isAfter date today)))
                (iterate (fn [date] (.plus date 2 (ChronoUnit/MONTHS))) start-date))))

(defn local-date-to-instant [local-date]
  (.toInstant (.atStartOfDay local-date) (ZoneOffset/UTC)))

(defn get-dates-in-instant-format []
  (map #(-> % local-date-to-instant .toString) (get-dates)))

(defn get-months [start-year end-year]
  (let [start-date (LocalDate/of start-year 1 1)
        end-date (LocalDate/of (inc end-year) 1 1) ; This will allow us to capture all months of the end year
        iso-format (DateTimeFormatter/ofPattern "yyyy-MM-dd")]
    (->> (iterate (fn [date] (.plus date 1 (ChronoUnit/MONTHS))) start-date)
         (map (fn [date]
                (let [start-str (.format date iso-format)
                      end-str (.format (.plus date 1 (ChronoUnit/MONTHS)) iso-format)]
                  {:start start-str, :end end-str})))
         (take-while #(not (.isAfter (LocalDate/parse (:start %) iso-format) end-date))))))

(defn convert->params [list-of-dates]
  (mapv (fn [{:keys [start end]}]
          (str "created:" start ".." end))
        list-of-dates))

(comment
  (deftest list-of-created-dates-test
    (testing "should return a list [...] "
      (let [dates (get-months 2020 2023)
            list-created (convert->params dates)]
        (is ["created:2020-01-01..2020-02-01"
             "created:2020-02-01..2020-03-01"
             "created:2020-03-01..2020-04-01"
             "created:2020-04-01..2020-05-01"
             "created:2020-05-01..2020-06-01"
             "created:2020-06-01..2020-07-01"
             "created:2020-07-01..2020-08-01"
             "created:2020-08-01..2020-09-01"
             "created:2020-09-01..2020-10-01"
             "created:2020-10-01..2020-11-01"
             "created:2020-11-01..2020-12-01"
             "created:2020-12-01..2021-01-01"
             "created:2021-01-01..2021-02-01"
             "created:2021-02-01..2021-03-01"
             "created:2021-03-01..2021-04-01"
             "created:2021-04-01..2021-05-01"
             "created:2021-05-01..2021-06-01"
             "created:2021-06-01..2021-07-01"
             "created:2021-07-01..2021-08-01"
             "created:2021-08-01..2021-09-01"
             "created:2021-09-01..2021-10-01"
             "created:2021-10-01..2021-11-01"
             "created:2021-11-01..2021-12-01"
             "created:2021-12-01..2022-01-01"
             "created:2022-01-01..2022-02-01"
             "created:2022-02-01..2022-03-01"
             "created:2022-03-01..2022-04-01"
             "created:2022-04-01..2022-05-01"
             "created:2022-05-01..2022-06-01"
             "created:2022-06-01..2022-07-01"
             "created:2022-07-01..2022-08-01"
             "created:2022-08-01..2022-09-01"
             "created:2022-09-01..2022-10-01"
             "created:2022-10-01..2022-11-01"
             "created:2022-11-01..2022-12-01"
             "created:2022-12-01..2023-01-01"
             "created:2023-01-01..2023-02-01"
             "created:2023-02-01..2023-03-01"
             "created:2023-03-01..2023-04-01"
             "created:2023-04-01..2023-05-01"
             "created:2023-05-01..2023-06-01"
             "created:2023-06-01..2023-07-01"
             "created:2023-07-01..2023-08-01"
             "created:2023-08-01..2023-09-01"
             "created:2023-09-01..2023-10-01"
             "created:2023-10-01..2023-11-01"
             "created:2023-11-01..2023-12-01"
             "created:2023-12-01..2024-01-01"
             "created:2024-01-01..2024-02-01"]
            list-created))))
  
  (list-of-created-dates-test)
  
  ;
  )
