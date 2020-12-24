(ns svg-to-html.svg.dom
  (:require [svg-to-html.svg.svg :as svg]
            [clojure.string :as str]))

(defn get-pixels [s]
  (when (string? s)
    (some->> (str/replace s #"px$" "")
             (re-find #"^\d+$")
             read-string)))

(defn get-max-images-width [x]
  (->> x
       flatten
       (filter map?)
       (map #(get-in % [:style :width]))
       (map get-pixels)
       (remove nil?)
       (concat [0])
       (apply max)))

(comment

  (get-max-images-width
   [:div
    {:style {:position :relative}}
    [:div.rect-9
     {:style
      {:top "676px"
       :background-color "black"
       :width "159px"
       :filter "url(#filter-10)"
       :opacity "1"
       :position "absolute"
       :height "344px"
       :left "499px"}}]
    [:img
     {:width "159"
      :height "344"
      :style
      {:top "676px"
       :width "159px"
       :position "absolute"
       :height "344px"
       :left "499px"}}]])



  )
