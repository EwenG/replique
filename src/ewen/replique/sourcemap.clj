(ns ewen.replique.sourcemap
  (:require [clojure.data.json :as json]
            [clojure.java.io :refer [as-file]])
  (:import [java.util Base64]
           [java.nio.charset StandardCharsets]
           [java.nio.file Paths]))

;;/*# sourceMappingURL=data:application/json;base64,ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJInJlc2V0LnNjc3MiCgldLAoJInNvdXJjZXNDb250ZW50IjogW10sCgkibWFwcGluZ3MiOiAiQUFHRSxHQUFHLENBQUMsQ0FBQyxDQUFIO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWDs7QUFLSCxDQUFDLENBQUMsQ0FBQyxDQUFEO0VBQ0EsS0FBSyxFQUFFLElBQUssR0FEWCIsCgkibmFtZXMiOiBbXQp9 */

(defn str->path [s]
  (if s
    (Paths/get s (make-array String 0))
    nil))

(defn decode-base-64 [s-b64]
  (if (nil? s-b64)
    nil
    (-> (Base64/getDecoder)
        (.decode s-b64)
        (String.))))

(defn encode-base-64 [s]
  (if (nil? s)
    nil
    (let [b (.getBytes s StandardCharsets/UTF_8)]
      (-> (Base64/getEncoder)
          (.encode b)
          (String.)))))

(def sourcemapping-regexp #"(?:\/\/[@#][ \t]+sourceMappingURL=([^\s'\"]+?)[ \t]*$)|(?:\/\*[@#][ \t]+sourceMappingURL=([^\*]+?)[ \t]*(?:\*\/)[ \t]*$)")
(def data-uri-regexp #"^data:[^;]*(?:;base64)?,(.*)$")

(defn json-read-str [s]
  (let [s (cond (nil? s) "null"
                (= "" (.trim s)) "null"
                :else s)]
    (json/read-str s)))

(def parse-data-uri
  (fnil
   (fn [s]
     (let [matcher (re-matcher data-uri-regexp s)]
       (second (re-find matcher))))
   ""))

(def parse-sourcemap
  (fnil
   (fn [s]
     (let [matcher (re-matcher sourcemapping-regexp s)]
       (-> (re-find matcher)
           (nth 2)
           parse-data-uri
           decode-base-64
           json-read-str)))
   ""))

(defn css-file->sourcemap [css-path]
  (if (.exists (as-file css-path))
    (-> (slurp css-path)
        parse-sourcemap
        (dissoc :sourcesContent :mappings))
    nil))

(defn data->sourcemap [data]
  (-> (parse-data-uri data)
      decode-base-64
      parse-sourcemap
      (dissoc :sourcesContent :mappings)))

(comment

  (parse-sourcemap "a {
  color: red; }

a {
  color: blue; }

p a {
  color: blue; }

/*# sourceMappingURL=data:application/json;base64,ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJInJ1bm5hYmxlcy9fcmVzZXQuc2NzcyIKCV0sCgkic291cmNlc0NvbnRlbnQiOiBbCgkJIkBpbXBvcnQgJ19yZXNldCc7XG5cbmEge1xuICAgIGNvbG9yOiBibHVlO1xuICB9XG5cbnAge1xuICBhIHtcbiAgICBjb2xvcjogYmx1ZTtcbiAgfVxufSIsCgkJImEge1xuICAgIGNvbG9yOiByZWQ7XG59XG4iCgldLAoJIm1hcHBpbmdzIjogIkFDQUEsQ0FBQyxDQUFDO0VBQ0UsS0FBSyxFQUFFLEdBQUksR0FEWjs7QURFSCxDQUFDLENBQUM7RUFDRSxLQUFLLEVBQUUsSUFBSyxHQURiOztBQUtELENBQUMsQ0FBQyxDQUFDLENBQUQ7RUFDQSxLQUFLLEVBQUUsSUFBSyxHQURYIiwKCSJuYW1lcyI6IFtdCn0= */
")

  )

(comment

  ;;sourceRoot: 'http://example.com/www/js/'
  (let [s (decode-base-64 "ewoJInZlcnNpb24iOiAzLAoJImZpbGUiOiAidGVzdC5jc3MiLAoJInNvdXJjZXMiOiBbCgkJInRlc3Quc2NzcyIsCgkJIi4uLy4uLy4uL3J1bm5hYmxlcy9fcmVzZXQuc2NzcyIKCV0sCgkic291cmNlc0NvbnRlbnQiOiBbCgkJIkBpbXBvcnQgJ19yZXNldCc7XG5cbmEge1xuICAgIGNvbG9yOiBibHVlO1xuICB9XG5cbnAge1xuICBhIHtcbiAgICBjb2xvcjogYmx1ZTtcbiAgfVxufSIsCgkJImEge1xuICAgIGNvbG9yOiByZWQ7XG59XG4iCgldLAoJIm1hcHBpbmdzIjogIkFDQUEsQ0FBQyxDQUFDO0VBQ0UsS0FBSyxFQUFFLEdBQUksR0FEWjs7QURFSCxDQUFDLENBQUM7RUFDRSxLQUFLLEVBQUUsSUFBSyxHQURiOztBQUtELENBQUMsQ0FBQyxDQUFDLENBQUQ7RUFDQSxLQUFLLEVBQUUsSUFBSyxHQURYIiwKCSJuYW1lcyI6IFtdCn0=")
        json-map (json-read-str s)
        sources (:sources json-map)
        sourceRoot (:sourceRoot json-map)]
    json-map
    #_sourceRoot
    #_s
    )

  )
