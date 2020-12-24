(ns test-tag
  (:require [reagent.core :as r]
            [garden.core :as gr]
            [garden.stylesheet :as gst]
            [garden.color :as gc]))

(def styles
  [:style
   (gr/css
    [:.contaner
     {:position :relative
      :margin "0px"}
     ])])

(defn page []
  [:div.container styles
      [:div
    [:div
     {:style
      {:width 100
       :height 100
       :border-radius "50%"
       :left "50px"
       :top "50px"}}]]])
