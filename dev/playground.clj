(ns playground
  (:require
   [svg-to-html.svg.parser :as parser]
   [svg-to-html.svg.transforms :as transforms]
   [clojure.java.io :as io]
   [hiccup.core :as hiccup]))

(comment

  (->> "dev/test-tag.svg"
       parser/parse-svg
       transforms/transform
       hiccup/html
       (spit (io/file "dev/test-tag.html")))


  (spit (io/file "dev/test-pattern-img.html")
        (str (->> "dev/test-pattern-img2.svg"
                  parser/parse-svg
                  transforms/transform
                  hiccup/html)
             "\n"
             (slurp "dev/test-pattern-img2.svg"))))
