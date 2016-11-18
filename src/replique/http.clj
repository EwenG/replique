(ns replique.http
  (:require [clojure.string :as str]))

;;; assumes first line already consumed
(defn parse-headers
  "Parse the headers of an HTTP POST request."
  [header-lines]
  (apply hash-map
    (mapcat
      (fn [line]
        (let [[k v] (str/split line #":" 2)]
          [(keyword (str/lower-case k)) (str/triml v)]))
      header-lines)))

(defn read-headers [rdr]
  (loop [next-line (.readLine ^java.io.BufferedReader rdr) header-lines []]
    (if (= "" next-line)
      header-lines ;; we're done reading headers
      (recur
        (.readLine ^java.io.BufferedReader rdr)
        (conj header-lines next-line)))))

(defn read-post [line rdr]
  (let [[_ path _] (str/split line #" ")
        headers1 (read-headers rdr)
        headers (parse-headers headers1)
        content-length (Integer/parseInt (:content-length headers))
        content (char-array content-length)]
    (io!
     (.read ^java.io.BufferedReader rdr content 0 content-length)
     {:method :post
      :path path
      :headers headers
      :content (read-string (String. content))})))

(defn read-get [line rdr]
  (let [[_ path _] (str/split line #" ")
        headers (parse-headers (read-headers rdr))]
    {:method :get
     :path path
     :headers headers}))

(defn read-request [rdr]
  (if-let [line (.readLine ^java.io.BufferedReader rdr)]
    (cond
      (.startsWith ^String line "POST") (read-post line rdr)
      (.startsWith ^String line "GET") (read-get line rdr)
      :else {:method :unknown :content line})
    {:method :unknown :content nil}))

(defn- status-line [^long status]
  (case status
    200 "HTTP/1.1 200 OK"
    404 "HTTP/1.1 404 Not Found"
    500 "HTTP/1.1 500 Error"
    "HTTP/1.1 500 Error"))

;; Same as cljs.repl/send-and-close but supports CORS
(defn send-and-close
  "Use the passed connection to send a form to the browser. Send a
  proper HTTP response."
  [out status ^String form content-type ^String encoding]
  (let [content-type (or content-type "text/html")
        encoding (or encoding "UTF-8")
        byte-form (.getBytes form encoding)
        content-length (count byte-form)
        headers (map #(.getBytes (str % "\r\n"))
                     [(status-line status)
                      "Server: ClojureScript REPL"
                      (str "Content-Type: "
                           content-type
                           "; charset=" encoding)
                      (str "Content-Length: " content-length)
                      (str "Access-Control-Allow-Origin: *")
                      (str "Access-Control-Allow-Methods: GET,POST")
                      ""])]
    (try
      (doseq [header headers]
        (.write ^java.io.OutputStream out header 0 (count header)))
      (.write ^java.io.OutputStream out byte-form 0 content-length)
      (.flush ^java.io.OutputStream out)
      (finally (.close ^java.io.OutputStream out)))))

(defn make-404 [path]
  {:status 404
   :body (str
          "<html><body>"
          "<h2>Page not found</h2>"
          "No page " path " found on this server."
          "</body></html>")
   :content-type "text/html"})

(def ext->mime-type
  {".html" "text/html"
   ".css" "text/css"

   ".jpg" "image/jpeg"
   ".png" "image/png"
   ".gif" "image/gif"
   ".ico" "image/x-icon"

   ".js" "text/javascript"
   ".json" "application/json"
   ".clj" "text/x-clojure"
   ".cljs" "text/x-clojure"
   ".cljc" "text/x-clojure"
   ".edn" "text/x-clojure"
   ".map" "application/json"})

(def mime-type->encoding
  {"text/html" "UTF-8"
   "text/css" "UTF-8"
   "image/jpeg" "ISO-8859-1"
   "image/png" "ISO-8859-1"
   "image/gif" "ISO-8859-1"
   "image/x-icon" "ISO-8859-1"
   "text/javascript" "UTF-8"
   "text/x-clojure" "UTF-8"
   "application/json" "UTF-8"})

(comment
  (read-request (clojure.java.io/reader (.toCharArray "POST / HTTP/1.1
Host: localhost:51223
Connection: keep-alive
Content-Length: 32
Origin: http://localhost:51223
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36
Content-Type: application/x-www-form-urlencoded;charset=UTF-8
Accept: */*
Referer: http://localhost:51223/
Accept-Encoding: gzip, deflate, br
Accept-Language: fr-FR,fr;q=0.8,en-US;q=0.6,en;q=0.4,es;q=0.2
Cookie: ONBANNERCOOKIE=; PLAY_SESSION= 1b2091db4608be37a0915c88537881ef7a097491-X-PARENT=f28b2e76-f594-46b0-9670-ab47ef7a0242; _ga=GA1.1.1667851944.1476435948; ABTasty=ABTastyUTMB%3A1%5E%7C%5ELiwioTracking%3A16101411054836054%5E%7C%5EsegmentationTracking%3A16101411054836054%5E%7C%5ELiwioUTMA%3A0.3.1476435948014.1477038183291.1477042908586.5

{:type :ready, :content \"ready\"}")))


  )
