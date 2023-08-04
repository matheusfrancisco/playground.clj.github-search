(ns chico.playground.core
  (:require [clj-http.client :as client])
  (:import [java.io FileOutputStream]))

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

(def url "https://github.com/stoyan-stoyanov/llmflows/archive/37ebb7d1cca32b99de88a44c1cfb5781b5389b34.zip")
(def file-path "/Users/matheus.machado/dev/m/playground.clj.github-search/yourfile.zip")

(download-zip url file-path)
