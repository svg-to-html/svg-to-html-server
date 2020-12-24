(ns svg-to-html.svg.util
  (:require #? (:cljs [goog.string.format])
            [clojure.string :as str]))

(defn drop-blanks [n]
  (let [res (cond
              (and (string? n) (str/blank? n)) nil
              (map? n)
              (let [nn (reduce (fn [acc [k v]]
                                 (let [nv (drop-blanks v)]
                                   (if (nil? nv)
                                     acc
                                     (assoc acc k nv))))
                               {} n)]
                (if (empty? nn) nil nn))

              (sequential? n)
              (let [nn (remove nil? (map drop-blanks n))]
                (if (empty? nn) nil (vec nn)))

              :else n)]

    res))

(defn uuid []
  (.toString (java.util.UUID/randomUUID)))
