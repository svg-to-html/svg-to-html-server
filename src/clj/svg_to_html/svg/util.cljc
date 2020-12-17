(ns svg-to-html.svg.util
  (:require #? (:cljs [goog.string.format])
            [clojure.string :as str]))

#?(:clj
   (defn regexp? [r]
     (instance? java.util.regex.Pattern r)))

(defn check-pred [m p]
  (->> p
       (map (fn [[k v]]
              (let [val (if (sequential? k)
                          (get-in m k)
                          (get m k))]
                (cond
                  (map? v) (check-pred val v)
                  (sequential? v) (some #(check-pred m {k %}) v)
                  (and val v (string? val) (regexp? v)) (re-find v val)
                  (and val v (keyword? val) (regexp? v)) (re-find v (name val))
                  (fn? v) (v val)
                  (= :not-nil? v) (not (nil? val))
                  :else (= val v)))))
       flatten
       (filter #(or (nil? %) (false? %)))
       empty?))

(defn assoc-path [m [k & ks] v]
  (cond
    (map? k)
    (let [m (or m [])]
      (if (some (fn [i] (= i (into i k))) m)
        (mapv (fn [i]
                (if (= i (into i k))
                  (assoc-path i ks v)
                  i)) m)
        (conj m (assoc-path k ks v))))

    (number? k)
    (let [new-v (assoc-path (get m k) ks v)]
      (if (< (dec (count m)) k)
        (conj (into (or m []) (vec (replicate (- k (count m)) nil)))
              new-v)

        (assoc m k new-v)))

    (and (= :* k))
    (mapv #(assoc-path nil ks %) v)

    (and (nil? k) (empty? ks)) v

    :else
    (let [value (assoc-path (get m k) ks v)]
      (assoc m k value))))

(defn get-path [m [k & ks]]
  (if k
    (if (= :* k)
      (let [res (mapv #(get-path % ks) m)]
        (if (empty? res) nil res))

      (get-path
       (cond
         (map? k) (some (fn [i] (and (= i (into i k)) i)) m)
         (number? k) (if (and (sequential? m) (< k (count m))) (nth m k) nil)
         :else (get m k))
       ks))
    m))

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

(def decode-url-string
  #?(:cljs js/decodeURIComponent
     :clj #(-> % java.net.URLDecoder (.decode "UTF8"))))

(defn parse-number [s]
  (cond
    (re-find #"^\d+\.\d+$" s) (#?(:cljs js/parseFloat :clj read-string) s)
    (re-find #"^\d+$" s)      (#? (:cljs js/parseInt :clj read-string) s)
    :else                   s))

(defn parse-query-string [s]
  (when s
   (->> (str/split s #"&")
        (map #(str/split % #"="))
        (map (fn [[k v]]
               (when-not (str/blank? k)
                 [(keyword k) (-> v
                                  decode-url-string
                                  parse-number)])))
        (remove nil?)
        (into {}))))

#?(:cljs
   (defn format
     "Formats a string using goog.string.format.
   e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply #?(:cljs goog.string/format :clj format) (concat [fmt] args))))

(defn value-cooef [v [start end :as range]]
  (if (number? v)
    (-> v
        (max start)
        (min end)
        double
        (- start)
        (/ (- (double end ) (double start))))
    0))

(defn add-indexes [coll]
  (->> coll
       (map-indexed
        (fn [idx x]
          (assoc x :index idx)))))

