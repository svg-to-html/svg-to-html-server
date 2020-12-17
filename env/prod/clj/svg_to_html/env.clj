(ns svg-to-html.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[svg-to-html started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[svg-to-html has shut down successfully]=-"))
   :middleware identity})
