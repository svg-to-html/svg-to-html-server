(ns svg-to-html.svg.transforms
  (:require [svg-to-html.svg.svg :as svg]
            [svg-to-html.svg.util :as util]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- round [x]
  (cond
    (nil? x) 0
    (number? x) x
    (string? x) (read-string x)
    :else x))

(defn- px [x]
  (str (round x) "px"))

(defn- fix-id [id]
  (-> id name (str/replace #"^#" "") keyword))

(defn body [x]
  (when (vector? x)
    (let [[_ _ _ body] (svg/tag-parts x)]
      body)))

(defn- single-tag? [x]
  (-> x body count (= 1)))

(defn- add-pixels [attrs]
  (->> attrs
       (map (fn [[k v]]
              [k
               (if (and
                    (#{:width :height :top :left :x :y :font-size :letter-spacing} k)
                    (-> v str (str/ends-with? "px") not))
                 (px v)
                 v)]))
       (into {})))

(defn- transform-to-pos [{:keys [transform] :as attrs}]
  (if-let [[x y] (:translate (svg/transform-str->map transform))]
    (-> attrs
        (dissoc :transform)
        (assoc :left (px x) :top (px y)))
   attrs))

(defn- add-class [tag-name id]
  (if (and id (keyword? id))
    (keyword (str (name tag-name) "." (name id)))
    tag-name))

(defn- add-id [tag-name id]
  (if (and id (keyword? id))
    (keyword (str (name tag-name) "#" (name id)))
    tag-name))

(defn- border-style [x]
  (-> x
      ((fn [{:keys [stroke stroke-width stroke-dasharray] :as x}]
         (if (and
              (nil? stroke-dasharray)
              (or (nil? stroke)
                  (= stroke "none")
                  (= stroke-width "0")))
           x
           (assoc x :border (str (if stroke-dasharray
                                   "1"
                                   (or stroke-width 1))
                                 "px" " "
                                 (if stroke-dasharray "dashed" "solid") " "
                                 stroke)))))
      ((fn [{:keys [rx] :as x}]
         (if rx
           (assoc x :border-radius (str rx "px"))
           x)))
      (dissoc :stroke :stroke-width :rx :stroke-linejoin :stroke-dasharray)))

(defn- attrs->style [attrs]
  {:style (-> attrs
              (transform-to-pos)
              ((fn [{:keys [x y] :as attrs}]
                 (if (or x y)
                   (assoc attrs :position "absolute")
                   attrs)))
              (add-pixels)
              (border-style)
              (clojure.set/rename-keys
               {:fill :background-color
                :fill-opacity :opacity
                :x :left :y :top})
              (dissoc :fill-rule :view-box :version :xlink-href :mask)
              ((fn [x]
                 (->> x
                      (remove #(-> % second (= "none")))
                      (into {})))))})

(defmulti transform-tag (fn [tag svg] (svg/tag->name tag)))

(defmethod transform-tag :svg [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t (add-class :div id)]
    [:div {:style {:position "relative"}}
     (util/drop-blanks
      (into
       [t (attrs->style attrs)]
       (->> body (map #(transform-tag % svg)))))]))

(defn inject-attrs [tag attrs]
  (if (svg/tag? tag)
    (let [[_ id attrs2 body] (svg/tag-parts tag)]
      (into [(first tag) (merge attrs attrs2)] body))
    tag))

(defmethod transform-tag :g [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t (add-class :div id)
        group-content (into
                       [(add-class :div id) {:style {:position "relative"}}]
                       (->> body (map
                                  #(transform-tag
                                    (inject-attrs % (dissoc attrs :transform ))
                                    svg))))]
    (if (:transform attrs)
      [:div {:style (merge {:position :absolute}
                           (transform-to-pos
                            (select-keys attrs [:transform])))}
       group-content]
      group-content)))

(defmethod transform-tag :rect [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t (add-class :div id)]
    (into
     [t (attrs->style attrs)]
     (->> body (map #(transform-tag % svg))))))

(defmethod transform-tag :text [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t (add-class :div id)
        shift (-> attrs :font-size)]
    (into
     [t {:data-svg "text"
         :style (->
                 attrs
                 (clojure.set/rename-keys {:fill :color})
                 add-pixels
                 (dissoc :stroke-width))}]
     (->> body
          (map (fn [x]
                 (let [[_ id attrs body] (svg/tag-parts x)]
                   [:div
                    {:data-svg "tspan"
                     :style (-> attrs
                                ((fn [{:keys [y font-size] :as o}]
                                   (assoc o :y (- (double (round y))
                                                  (double (round (or font-size shift)))))))
                                (clojure.set/rename-keys {:fill :color :x :left :y :top})
                                (select-keys [:font-family :font-size :font-weight :color
                                              :letter-spacing :top :left])
                                (assoc :position :absolute :white-space :nowrap)
                                add-pixels)}
                    (last body)])))))))

(defmethod transform-tag :tspan [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)]
    [:div
     {:style (-> attrs
                 (clojure.set/rename-keys {:fill :color :x :left :y :top})
                 (select-keys [:font-family :font-size :color :letter-spacing :top :left])
                 (assoc :position :absolute :white-space :nowrap)
                 add-pixels)}
     (last body)]))

(defmethod transform-tag :use [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        link-id (fix-id (:xlink-href attrs))]
    (transform-tag
     (let [tag (svg/find-tag-by-id svg link-id)]
       (inject-attrs tag attrs))
     svg)))

(defmethod transform-tag :image [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        base-64 (:xlink-href attrs)]
    [:img (merge
           {:src base-64}
           (transform-to-pos (select-keys attrs [:width :height]))
           (attrs->style (dissoc attrs :xlink-href)))]))

(defmethod transform-tag :path [tag svg])
(defmethod transform-tag :circle [tag svg])
(defmethod transform-tag :ellipse [tag svg])
(defmethod transform-tag :pattern [tag svg])
(defmethod transform-tag :mask [tag svg])
(defmethod transform-tag :default [tag svg])

(defn- relative-div? [x]
  (when (vector? x)
   (let [[_ _ attrs _] (svg/tag-parts x)]
     (= attrs {:style {:position "relative"}}))))

(defn- inline-divs [dom]
  (walk/postwalk
   (fn [x]
     (if (and (single-tag? x) (relative-div? x))
       (-> x body first)
       x))
   dom))

(defn transform [svg]
  (-> (transform-tag svg svg)
      inline-divs))

(comment

  

  (svg-to-html.svg.core/svg->cljs
   "resources/svg/test.svg"
   "src/cljs/svg_to_html/test_dom.cljs"
   "svg-to-html.test-dom")




  )
