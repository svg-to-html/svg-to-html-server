(ns svg-to-html.app
  (:require [svg-to-html.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init! false)
