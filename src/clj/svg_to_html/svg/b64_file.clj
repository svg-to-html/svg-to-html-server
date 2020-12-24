(ns svg-to-html.svg.b64-file
  (:require [clojure.java.io :as io]
            [clojure.data.codec.base64 :as b64-codec]))

(defn- decode-str [s]
  (b64-codec/decode (.getBytes s)))

(defn- chop-header [s]
  (nth (first (re-seq #"(data:image/.*;base64,)(.*)" s)) 2))

(defn b64-ext [s]
  (second (first (re-seq #"data:image/(.*);base64.*" s))))

(defn write-img! [b64 f]
  (when (.exists f)
   (io/delete-file f))
  (io/copy
   (decode-str (chop-header b64))
   f))

