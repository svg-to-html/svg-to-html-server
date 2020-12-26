(ns svg-to-html.svg.dom
  (:require [svg-to-html.svg.svg :as svg]
            [clojure.string :as str]))

(defn get-pixels [s]
  (when (string? s)
    (some->> (str/replace s #"px$" "")
             (re-find #"^\d+$")
             read-string)))

(defn get-max-images-width [x & [left]]
  (->> x
       (map (fn [x]
              (if (vector? x)
                (let [[t attrs & body] x
                      current-left (+ (or (:left attrs) 0) (or left 0))]
                  (into
                   [(+ current-left (or (some-> attrs :width get-pixels) 0))]
                   (->> body (map #(get-max-images-width % current-left))))))))
       flatten
       (remove nil?)
       (concat [0])
       (apply max)))


