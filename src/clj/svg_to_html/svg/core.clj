(ns svg-to-html.svg.core
  (:require [svg-to-html.svg.svg :as svg]
            [svg-to-html.svg.parser :as parser]
            [svg-to-html.svg.transforms :as transforms]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn- make-indent-str [n]
  (->> (cycle " ")
       (take (or n 0))
       (str/join "")))

(defn- pretty-str [x & [indent]]
  (->>
   (with-out-str (pp/pprint x))
   str/split-lines
   (map #(str (make-indent-str indent) %))
   (map #(str/replace % #",$" ""))
   (str/join "\n")))

(defn svg->cljs [svg-f clj-f namespace]
  (let [clj-f (io/file clj-f)
        dom (->> svg-f
                 parser/parse-svg
                 transforms/transform)
        template (slurp (io/resource "templates/html-namespace.txt"))
        dom-str  (pretty-str dom 3)
        content  (format template namespace "" dom-str)]
    (clojure.java.io/make-parents clj-f)
    (spit (io/file clj-f) content)))

(comment

  (svg->cljs "resources/svg/test-2.svg"
             "src/cljs/svg_to_html/test_dom.cljs"
             "svg-to-html.test-dom"))
