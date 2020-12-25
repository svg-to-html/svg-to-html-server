(ns svg-to-html.svg.parser
  (:require [hiccup-bridge.core :as hicv]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [camel-snake-kebab.core :as keb :refer [->camelCase ->kebab-case ->kebab-case-keyword]]
            [pl.danieljanus.tagsoup :as tagsoap]
            [clojure.string :as str]))

(def fix-keys {:viewbox :viewBox
               :filterunits :filterUnits
               :stddeviation :stdDeviation
               :xlink:href :xlinkHref
               :patternunits :pattern-units})

(defn tag->id [x]
  (some-> x name (str/split #"#") second))

(defn tag->name [x]
  (some-> x name (str/split #"#") first))


(defn- remove-buildins [s]
  (-> s
      (str/replace #"^(.+)(#Shape|#Group|#Oval|#Rectangle|#Combined|#Path).*" "$1")))

(defn optimize-id [s]
  (-> s
      (str/trim)
      (str/replace #"\s" "-")
      (str/replace #"[:]$" "")
      (str/replace #"[,$]" "")
      (str/replace #"[\-_]+" "-")
      ->kebab-case))

(defn- optimize-tag-name [s]
  (if-let [id (tag->id s)]
    (str (tag->name s) "#" (optimize-id id))
    s))

(defn fix-tag-name [tag]
  (-> tag
      name
      remove-buildins
      optimize-tag-name
      keyword))

(defn link? [s]
  (and
   (str/starts-with? s "url(#")
   (str/ends-with? s ")")))

(defn fix-link [s]
  (if (link? s)
    (str
     "url(#"
     (-> (re-find #"#(.+)\)" s)
         second
         optimize-id)
     ")")
    s))

(defn transformations [m]
  (walk/postwalk (fn [x]
                   (cond
                     (map? x) (->> x
                                   (map (fn [[k v]]
                                          [(->kebab-case-keyword (or (get fix-keys k) k)) v]))
                                   (map (fn [[k v]]
                                          (if (and
                                               (string? v)
                                               (str/starts-with? v "url(#"))
                                            [k (fix-link v)]
                                            [k v])))
                                   (into {}))
                     (vector? x) (update x 0 fix-tag-name)
                     :else x)) m))

(defn svg->hiccup [s]
  (-> s
   hicv/html-file->hiccup
   first))

(defn svg->hiccup2 [s]
  (-> s tagsoap/parse))

(defn svg->hiccup3 [s]
  (-> (slurp s)
      hicv/html->hiccup
      first))

(defn parse-svg [svg-file]
  (-> svg-file
      svg->hiccup
      transformations))

(defn dump-svg-file [svg-file namespace]
  (let [path (str "src/cljs/art/" namespace  ".cljs")]
    (clojure.java.io/make-parents path)
    (spit
     path
     (with-out-str
       (println (str "(ns art." namespace ")"))
       (println (str "(def " namespace "-art"))
       (pp/pprint (parse-svg svg-file))
       (println ")")))))

(comment
  (dump-svg-file "resources/svg/test.svg" "test-svg")

  )
