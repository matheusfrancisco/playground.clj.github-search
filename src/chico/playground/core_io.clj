(ns chico.playground.core-io 
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(defn read-edn
  [file]
  (let [resource (io/resource file)]

    (if (nil? resource)
      (throw (Exception. (str "File not found: " file))))
    (with-open [rdr (io/reader resource)]
      (edn/read-string (slurp rdr)))))
  

