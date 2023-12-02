(ns chico.playground.core
  (:require
   [chico.playground.utils :refer [convert->params get-dates-in-instant-format
                                   get-months]]
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [clojure.walk :as w])
  (:import
   [java.io FileOutputStream]
   [java.time Duration]
   java.time.Instant))

(defn download-zip [url file-path]
  (try
    (let [response (client/get url {:as :byte-array})
          status (:status response)
          content-disposition (get-in response [:headers "content-disposition"])]
      (if (and (= 200 status) (some? (re-find #"attachment;" content-disposition)))
        (with-open [out (FileOutputStream. file-path)]
          (.write out (:body response))
          (println "Downloaded" url "to" file-path))
        (println "Failed to download" url "with status" status)))
    (catch Exception e
      (println "An error occurred while downloading" url ":" (.getMessage e)))))


(defn download-repos [repos file-path]
  (mapv
   (fn [item]
     (pprint/pprint (str file-path (:name item)))
     (download-zip (:download_url item) (str file-path (:name item))))
   repos))


#_(defn find-clojure-repos []
    (iteration
     (with-retries
       (fn [{:keys [url num-stars last-response] :as k}]
         (prn (select-keys k [:url :num-stars]))
         (let [req
               (cond
               ;; initial request
                 (nil? k) (search-repos-request "language:clojure")

               ;; received next-url
                 url (assoc base-request
                            :url url)

               ;; received star number
                 num-stars (search-repos-request (str "language:clojure " "stars:" num-stars))

                 :else (throw (Exception. (str "Unexpected key type: " (pr-str k)))))]
           (rate-limit-sleep! last-response)
           (let [response (http/request (with-auth req))]
             (assoc response
                    ::key k
                    ::request req)))))
     :kf
     (fn [response]
       (let [num-stars (-> response
                           ::key
                           :num-stars)
             url (-> response :links :next :href)]
         (if url
           {:last-response response
            :url url
            :num-stars num-stars}
           (if num-stars
             (let [next-num-stars (dec num-stars)]
               (when (>= next-num-stars 1)
                 {:num-stars next-num-stars
                  :last-response response}))
             (let [num-stars (-> response
                                 :body
                                 :items
                                 last
                               ;; want to continue from where we left off
                                 :stargazers_count)]
               {:num-stars num-stars
                :last-response response})))))
;   :initk {:num-stars 50}
     ))



