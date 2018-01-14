(ns replique.cljs-env.elisp-printer)


(defmacro print-with-meta [o w & body]
  `(let [m# (meta ~o)]
     (if (and (pos? (count m#)) *print-meta*)
       (do
         (-write ~w "[\"~#with-meta\" [")
         ~@body
         (-write ~w " ")
         (-pr-writer m# ~w)
         (-write ~w "]]"))
       (do ~@body))))
