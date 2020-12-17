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
               :xlink:href :xlinkHref})

(defn tag->id [x]
  (some->> x name (re-find #"#(.+$)") second))

(defn fix-tag-name [tag]
  (if (when-let [tag-name (tag->id tag)]
       (or
        (re-find #"----" tag-name)
        (str/includes? tag-name " ")
        (not= tag-name (str/lower-case tag-name))))
    (->> tag name (re-find #"^(.+)#.+$") second keyword)
    tag))

(defn transform-keys [m f]
  (let [fm (fn [[k v]] [(f (or (get fix-keys k) k)) v])]
    (walk/postwalk (fn [x]
                     (cond
                       (map? x) (into {} (map fm x))
                       (vector? x) (update x 0 fix-tag-name)
                       :else x)) m)))

(defn all-keys-camel-to-dash [m]
  (transform-keys m ->kebab-case-keyword))

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
      all-keys-camel-to-dash))

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
