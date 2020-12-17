(ns emails.pages.self-enrollment-email
  (:require [reagent.core :as r]
            [garden.core :as garden]
            [garden.stylesheet :as stylesheet]))

(def styles
  [:style
   (garden/css
    [:.contaner
     {}
     ])])

(defn page []
  [:div.container styles
   ])
