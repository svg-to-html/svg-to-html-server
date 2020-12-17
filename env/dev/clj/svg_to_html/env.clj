(ns svg-to-html.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [svg-to-html.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[svg-to-html started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[svg-to-html has shut down successfully]=-"))
   :middleware wrap-dev})
