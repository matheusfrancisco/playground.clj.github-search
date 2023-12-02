(ns chico.playground.search
  (:require
   [chico.playground.utils :as utils]
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.walk :as w])
  (:import
   [java.time Duration]
   java.time.Instant))

(def base-url-repo "https://api.github.com/search/repositories?q=language:Clojure+")
(def db (atom []))

(defn transform-map
  [data-map transform-map]
  (->> (keys transform-map)
       (reduce (fn [acc k]
                 (if (contains? data-map k)
                   (update-in acc [k] (transform-map k))
                   acc))
               data-map)))

(defn wait-rate-limit!
  [response]
  (let [headers (:headers response)
        data-map (select-keys headers ["X-RateLimit-Reset" "X-RateLimit-Remaining"])
        fn-map {"X-RateLimit-Remaining" (fn [remaining] (Long/parseLong remaining))
                "X-RateLimit-Reset" (fn [number-of-seconds]
                                      (-> number-of-seconds
                                          (Long/parseLong)
                                          (Instant/ofEpochSecond)))}
        t (w/keywordize-keys (transform-map data-map fn-map))]
    (prn :tt t)
    (when (<= (:X-RateLimit-Remaining t) 0)
      (let [duration (Duration/between (Instant/now)
                                       (:X-RateLimit-Reset t))
            duration-ms (+ 1000 (.toMillis duration))]
        (when (pos? duration-ms)
          (prn
           ":>> sleeping " duration-ms)
          (Thread/sleep duration-ms))))))

(defn make-urls [params-created per-page start-page]
  (mapv
   (fn [params-created]
     (str base-url-repo
          params-created
          "&per_page=" per-page
          "&page=" start-page "&order=asc"))
   params-created))

(defn request [url]
  (let [token "token"]
    (try
      (client/get url
                  {:headers {"Authorization: " (str  "Bearer " token)
                             "Accept" "application/vnd.github.v3+json"}})
      (catch Exception e
        (println "An error occurred" url ":" (.getMessage e))))))

(defn with-retries [step]
  (fn [m]
    (let [result
          (try
            (step m)
            (catch Exception e
              (pp/pprint e)
              ::error))]
      (if (= result ::error)
        (let [error-count (get m ::error-count 0)]
          (if (< error-count 3)
            (do
              ;; sleep for a second and then retry
              (prn "received 50X error. retrying...")
              (Thread/sleep 1000)
              (recur (assoc m ::error-count (inc error-count))))
            (throw (ex-info "Failed after retries"
                            {:k m}))))
        result))))

(defn replace-url-page [url new-page]
  (let [base-url (re-find #"(.*page=)[0-9]+(.*)" url)]
    (if base-url
      (str (base-url 1) new-page (base-url 2))
      url)))

(defn save-repos-items [r fname]
  (with-open  [wtr (io/writer (io/file fname))]
    (.write wtr (pr-str r))))

(defn repos [year-start year-end]
  (let [params-created (utils/convert->params
                        (utils/get-months year-start year-end))

        #_#_s created:2023-07-01..2023-08-13&per_page=100&page=1&order=asc
        per-page 100
        start-page 1
        urls (make-urls params-created per-page start-page)]
    urls))

(defn clojure-repos-first-pages [items]
  (loop [urls items
         rest-of-urls []]
    (prn :left-here-first (count urls))

    (if (empty? urls)
      (do 
        (prn :get-all-sub-urls)
        rest-of-urls)
      (let [first-url (first urls)
            response (request first-url)
            body (json/read-str (:body response)
                                :key-fn keyword)
            total-items (:total_count body)
            pages (/ total-items 100)

            n (mapv
               (fn [page] (replace-url-page first-url page))
               (range 2 (inc pages)))
            _ (prn :number-new-pages n)

            items (map (fn [item]
                         {:total-items total-items
                          :id (:id item)
                          :url-commit (str/replace (:commits_url item) "{/sha}" "/master")
                          :url (:html_url item)
                          :owner (-> item :owner :login)
                          :description (:description item)
                          :name (:name item)
                          :download-url (str (:html_url item) "/archive/master.zip")})
                       (:items body))]
        (swap! db concat items)
        (wait-rate-limit! response)
        (recur (rest urls)
               (concat rest-of-urls n))))))

(defn get-all-clojure-repos-between-save-io [urls path]
  (let [left-urls (clojure-repos-first-pages urls)]
    (loop [urls left-urls]
      (prn :left (count urls))
      (if (empty? urls)
        (do 
          #_(save-repos-items @db path)
          (prn :done))
        (let [first-url (first urls)
              response (request first-url)
              body (json/read-str (:body response)
                                  :key-fn keyword)
              total-items (:total_count body)
              items (map (fn [item]
                           {:total-items total-items
                            :id (:id item)
                            :url-commit (str/replace (:commits_url item) "{/sha}" "/master")
                            :url (:html_url item)
                            :owner (-> item :owner :login)
                            :description (:description item)
                            :name (:name item)
                            :download-url (str (:html_url item) "/archive/master.zip")})
                         (:items body))]
          (swap! db concat items)
          (wait-rate-limit! response)
          (recur (rest urls)))))))



(comment 
  (get-all-clojure-repos-between-save-io (repos 2011 2023) "resources/output.edn")

  (take 2 (slurp (io/resource "output.edn")))
  )

(comment
  (def url-test "https://api.github.com/search/repositories?q=language:Clojure+created:2023-12-01..2024-01-01&per_page=100&page=1&order=asc")
  (replace-url-page url-test 2)

  (pp/pprint (repos 2020 2023))
  ;(clojure.pprint/pprint (do-request (repos 2020 2023)))

  (def months-list (utils/get-months 2020 2023))
  ;(clojure.pprint/pprint (utils/convert->params months-list))
  (def url  "https://api.github.com/search/repositories?q=git+language:Clojure+created:%3E=2023-06-11T00:00:00Z&order=asc")
;
  )

(comment
  (require '[clojure.test :as t])

  (t/deftest tests
    (t/testing "testing transform-map"
      (let [data-map {:a 1 :b 2 :c 3}
            fn-map {:a inc :b dec}]
        (t/is (= {:a 2 :b 1 :c 3}
                 (transform-map data-map fn-map)))))

    (t/testing "get and apply values"
      (let [data-map
            {"X-RateLimit-Remaining" "9",
             "X-RateLimit-Reset" "1691880860"}
            fn-map {"X-RateLimit-Remaining" (fn [v] (Long/parseLong v))
                    "X-RateLimit-Reset" (fn [v]
                                          (-> v
                                              (Long/parseLong)
                                              (Instant/ofEpochSecond)
                                              (.toString)))}]
        (t/is (= {"X-RateLimit-Remaining" 9,
                  "X-RateLimit-Reset" "2023-08-12T22:54:20Z"}
                 (transform-map data-map fn-map))))))

  (tests)
  ;
  )

(comment
  "https://api.github.com/search/repositories?q=language:clojure&page=1&per_page=100&created%3E=2023-06-11T00:00:00Z&sort=created&order=asc"
  "https://api.github.com/search/repositories?q=language:Clojure+created:%3E=2023-06-11T00:00:00Z&order=asc"
  (def url  "https://api.github.com/search/repositories?q=git+language:Clojure+created:%3E=2023-06-11T00:00:00Z&order=asc")

  #_(let [repos  (search-github-repos url)
          path "/home/matheusfrancisco/m/dev/playground.clj.github-search/downloads/"]
      (download-repos repos path)))
