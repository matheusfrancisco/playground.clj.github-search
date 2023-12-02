(ns chico.playground.d
  (:import
   [clojure.lang LispReader]
   [java.io
    BufferedInputStream
    ByteArrayOutputStream
    ObjectOutputStream
    PushbackReader
    StringReader]
   [java.net URL]
   [java.util.zip ZipInputStream]))

#_(defn read-zip-contents [zip-stream]
  (let [zis (ZipInputStream. zip-stream)
        entries (atom {})]
    (loop []
      (let [entry (.getNextEntry zis)]
        (when entry
          (let [baos (java.io.ByteArrayOutputStream.)
                buffer (byte-array 4096)
                name (.getName entry)]
            (loop []
              (let [len (.read zis buffer)]
                (when (> len 0)
                  (.write baos buffer 0 len)
                  (recur))))
            (swap! entries assoc name (.toByteArray baos))
            (.closeEntry zis)))
        (when entry (recur))))
    @entries))

#_(defn read-zip-contents [zip-stream]
  (let [zis (ZipInputStream. zip-stream)
        entries (atom {})]
    (loop []
      (let [entry (.getNextEntry zis)]
        (when entry
          (let [baos (java.io.ByteArrayOutputStream.)
                buffer (byte-array 4096)
                name (.getName entry)]
            (loop []
              (let [len (.read zis buffer)]
                (when (> len 0)
                  (.write baos buffer 0 len)
                  (recur))))
            (swap! entries assoc name (String. (.toByteArray baos) "UTF-8"))
            (.closeEntry zis)))
        (when entry (recur))))
    @entries))

#_(defn download-zip-in-memory [url]
  (with-open [input-stream (-> (URL. url) .openStream BufferedInputStream.)]
    (read-zip-contents input-stream)))

;; Example usage
#_(def zip-contents (download-zip-in-memory "https://github.com/nivekuil/scv/archive/master.zip"))


(defn read-all-forms [s]
  (let [reader (PushbackReader. (StringReader. s))]
    (loop [forms []]
      (let [form (try (LispReader/read reader false nil false)
                      (catch Exception _ nil))]
        (if (nil? form)
          forms
          (recur (conj forms form)))))))

(defn is-def? [form]
  (and (seq? form)
       (some #{'def 'defn 'defmacro} (take 1 form))))

(defn extract-defs [ns-string]
  (filter is-def? (read-all-forms ns-string)))


(defn serialize-object-to-bytes [obj]
  (with-open [baos (ByteArrayOutputStream.)
              oos (ObjectOutputStream. baos)]
    (.writeObject oos obj)
    (.flush oos)
    (.toByteArray baos)))

(defn size-in-mb [obj]
  (/ (count (serialize-object-to-bytes obj)) 1048576.0))

;; Example usage
(def size-mb (size-in-mb zip-contents))

(defn read-all-forms [s]
  (let [reader (java.io.PushbackReader. (java.io.StringReader. s))]
    (loop [forms []]
      (let [form (try (read reader false nil)
                      (catch Exception _ nil))]
        (if (nil? form)
          forms
          (recur (conj forms form)))))))

(defn load-ns-from-string [ns-string]
  (doseq [form (read-all-forms ns-string)]
    (eval form)))


(defn parse-ns-name [ns-string]
  (let [reader (LispReader/read (PushbackReader. (StringReader. ns-string)) false nil false)]
    (when (= 'ns (first reader))
      (second reader))))

(def ns-valid "(ns  test.chico) (def x 1)")

#_(load-ns-from-string (get zip-contents "scv-master/src/com/nivekuil/scv.clj"))
#_(load-ns-from-string ns-valid)
#_(prn test.chico/x) ;=> 1

#_(parse-ns-name (get zip-contents "scv-master/src/com/nivekuil/scv.clj"))


(defn read-entry [zip-stream entry]
  (let [baos (ByteArrayOutputStream.)
        buffer (byte-array 4096)]
    (loop []
      (let [len (.read zip-stream buffer)]
        (when (> len 0)
          (.write baos buffer 0 len)
          (recur))))
    (String. (.toByteArray baos) "UTF-8")))

(defn read-zip-contents [zip-stream]
  (loop [zis zip-stream
         entries {}]
    (let [entry (.getNextEntry zis)]
      (if entry
        (let [entry-name (.getName entry)
              entry-content (read-entry zis entry)]
          (.closeEntry zis)
          (recur zis (assoc entries entry-name entry-content)))
        entries))))

(defn download-zip-in-memory [url]
  (with-open [input-stream (-> (URL. url) .openStream BufferedInputStream.)]
    (read-zip-contents (ZipInputStream. input-stream))))

(def zip-contents (download-zip-in-memory "https://github.com/nivekuil/scv/archive/master.zip"))


(defn add-path [tree [path content]]
  (let [parts (clojure.string/split path #"/")]
    (reduce (fn [acc part]
              (let [current-path (take-while (partial not= part) parts)]
                (if (and (= part (last parts)) (not-empty content))
                  (assoc-in acc (concat current-path [part]) content)
                  (if (get-in acc (concat current-path [part]))
                    acc
                    (assoc-in acc (concat current-path [part]) {})))))
            tree
            parts)))

(defn paths-to-tree [path-map]
  (reduce add-path {} path-map))

(def tree (paths-to-tree zip-contents))
(println tree)

(extract-defs (get zip-contents "scv-master/src/com/nivekuil/scv.clj"))
