(ns svg-to-html.svg.svg
  (:require [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn tag?
  ([x] (and (vector? x) (some-> x first keyword?)))
  ([x tag-name]
   (and (tag? x)
        (some-> x first name (str/starts-with? (name tag-name))))))

(defn- tag [x]
  (cond
    (tag? x) (first x)
    (keyword? x) x
    :else nil))

(defn tag->id [x]
  (some->> x tag name (re-find #"#(.+$)") second keyword))

(tag->id {:a :b})

(defn tag->name [x]
  (some-> x tag name (str/replace #"#.+$" "") keyword))

(defn add-empty-attrs [x]
  (if (map? (nth x 1 nil))
    x
    (vec (concat [(first x)] [{}] (rest x)))))

(defn tag-parts [tag]
  (let [tag (add-empty-attrs tag)
        [name attrs & body] tag]
   [(tag->name tag)
    (tag->id tag)
    attrs
    body]))

(defn parse-number [s]
  (cond
    (re-find #"^-?\d+\.\d+$" s) (read-string s)
    (re-find #"^-?\d+$" s)      (read+string s)
    :else                   s))

(defn- parse-transform-value [s]
  (let [parts (->> (str/split s #"\s*,\s*")
                   (map parse-number))]
    (if (-> parts count (= 1))
      (first parts)
      (vec parts))))

(defn- value-merge-fn [a b]
  (if (and (sequential? a) (sequential? b))
    (map + a b)
    (+ a b)))

(defn transform-str->map [s]
  (if (str/blank? s)
    {}
    (apply merge-with
     (concat
      [value-merge-fn]
      (->> s
           (re-seq #"\b(.+?)\((.+?)\)")
           (map rest)
           (map (fn [[k v]]
                  {(keyword k)
                   (parse-transform-value v)})))))))

(defn get-tag-attributes [svg]
  (-> svg
      add-empty-attrs
      second))

(defn get-tag-attribute [svg k]
  (-> svg
      get-tag-attributes
      (get k)))

(defn find-tag [svg tag-name]
  (when (tag? svg)
    (or
     (when (tag? svg tag-name)
       svg)
     (->> svg
          (filter #(tag? % tag-name))
          first)
     (->> svg
          (map #(find-tag % tag-name))
          (remove nil?)
          first))))

(defn find-tag-by-id [svg tag-id]
  (when (tag? svg)
    (or
     (when (-> svg tag->id (= tag-id))
       svg)
     (->> svg
          (filter #(-> % tag->id (= tag-id)))
          first)
     (->> svg
          (map #(find-tag-by-id % tag-id))
          (remove nil?)
          first))))

(defn find-value [svg k]
  (or (when (and
             (map? svg)
             (contains? svg k))
        (get svg k))
      (when (sequential? svg)
        (->> svg
             (map #(find-value % k))
             (remove nil?)
             first))))

(defn get-svg-pos [svg]
  (if (tag? svg :rect)
    {:x (-> svg (find-value :x) parse-number)
     :y (-> svg (find-value :y) parse-number)}
    (let [[x y] (some-> svg
                        (get-tag-attribute :transform)
                        transform-str->map
                        :translate)]
      {:x x :y y})))

(defn merge-pos [& pos]
  (apply merge-with (concat [+] pos)))

(defn get-svg-bounds [svg]
  (merge
   {:x      (find-value svg :x)
    :y      (find-value svg :y)
    :width  (find-value svg :width)
    :height (find-value svg :height)}
   (get-svg-pos svg)))


