(ns svg-to-html.svg.transforms
  (:require [svg-to-html.svg.svg :as svg]
            [svg-to-html.svg.util :as util]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hiccup.core :as h]
            [svg-to-html.svg.b64-file :as b64-file]
            [svg-to-html.svg.dom :as dom]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def ^:dynamic config {:base-dir "resources/public"
                       :img "gen-img"})

(def defs (atom {}))

(defn get-image-file-path [id ext]
  (str (:img config) "/" (or (some-> id name) (util/uuid)) "." ext))

(defn save-base64 [id base-64]
  (let [file-path  (get-image-file-path id (b64-file/b64-ext base-64))
        file (io/file (str (:base-dir config) "/" file-path))]
    (io/make-parents file)
    (b64-file/write-img! base-64 file)
    file-path))

(defn save-svg [id content]
  (let [file-path  (get-image-file-path id "svg")
        file (io/file (str (:base-dir config) "/" file-path))]
    (io/make-parents file)
    (spit file content)
    file-path))

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

(defn- transform-filter [{filter-ref :filter :as attrs}]
  (if filter-ref
    (let [filter-name (->> filter-ref
                           (re-matches  #"url\(#(.*)\)")
                           second
                           keyword)
          filter-tag  (get @defs filter-name)]

      (transform-tag filter-tag {}))))

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
      ((fn [{:keys [cx cy rx ry r] :as x}]
         (if (and cx cy)
           (let [sizes (cond
                         r (mapv read-string [r r])
                         (and rx ry) (mapv read-string [rx ry]))
                 [w h] (mapv (partial * 2) sizes)
                 [cx cy] (mapv read-string [cx cy])
                 [left top] (mapv - [cx cy] sizes)]
             (-> x
                 (assoc :width (str w "px"))
                 (assoc :height (str h "px"))
                 (assoc :border-radius "50%")
                 (assoc :left (str left "px"))
                 (assoc :top (str top "px"))))
           x)))
      (dissoc :stroke :stroke-width :rx :ry :r :cx :cy :stroke-linejoin :stroke-dasharray)))

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
                {:fill         :background-color
                 :fill-opacity :opacity
                 :x            :left :y :top})
              (dissoc :fill-rule :view-box :version :xlink-href :mask)
              ((fn [x]
                 (->> x
                      (remove #(-> % second (= "none")))
                      (into {}))))
              ((fn [x]
                 (if-let [[_ id] (some->> (:background-color x)
                                          (re-matches #"url\(#(.+)\)"))]
                   (-> x
                       (dissoc :background-color)
                       (assoc :background-image (format "url(%s)" (get-image-file-path id "svg"))))
                   x))))})

(defn shadow? [tag]
  (let [[tag-name _ attrs body] (svg/tag-parts tag)]
    false
    #_(and
        (= :g tag-name)
        (:filter attrs))))

(defmulti transform-tag
  (fn [tag svg]
    (cond
      (shadow? tag) :shadow
      :else (svg/tag->name tag))))

(defmethod transform-tag :shadow [tag svg]
  nil)


(defmethod transform-tag :svg [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t                 (add-class :div id)]
    [:div {:style {:position "relative"}}
     (util/drop-blanks
       (into
         [t (attrs->style attrs)]
         (->> body (map #(transform-tag % svg)))))]))

(defn str->color-matrix [cm-string]
  (str/split cm-string #"\s+"))

;;TODO: make host color multiplication
(defn color-matrix->rgba [color-matrix]
  (let [rgba-vec (mapv color-matrix [0 6 12 18])]
    ;;XXX this is a temporary solution
    (str "rgba(0,0,0," (last rgba-vec) ")")))

(defmethod transform-tag :filter [tag svg]
  (let [[_ _ _ body] (svg/tag-parts tag)
        filter-data  (->> body
                          (map #(transform-tag % svg))
                          (reduce merge)
                          )
        ;;XXX: workaround because css filter somehow clashes with svg filter
        ;;filter-data  (assoc filter-data :box-shadow true)
        style-vec    (if (:box-shadow filter-data)
                       (-> filter-data
                           (update :color-matrix color-matrix->rgba)
                           (clojure.set/rename-keys {:color-matrix :color})
                           ((juxt :offset-x :offset-y :blur-radius :color)))
                       (let [opacity (-> filter-data :color-matrix (get 18) read-string (* 100))
                             blur    (:blur-radius filter-data)]
                         [(str "blur(" blur ")") (str "opacity(" opacity "%)")]))
        style        (->> style-vec (interpose " ") (apply str))]
    (prn "style:" style)
    (if (:box-shadow filter-data)
      {:box-shadow style}
      {:filter style})))

(defmethod transform-tag :feOffset [tag svg]
  (let [[_ _ attrs _] (svg/tag-parts tag)]
    (-> attrs
        (select-keys [:dx :dy])
        (util/update-map px)
        (clojure.set/rename-keys {:dx :offset-x, :dy :offset-y}))))

(defmethod transform-tag :feGaussianBlur [tag svg]
  (let [[_ _ attrs _] (svg/tag-parts tag)
        update-all    #(reduce-kv (fn [m k v]))]
    (-> attrs
        (select-keys [:std-deviation])
        (update :std-deviation px)
        (clojure.set/rename-keys {:std-deviation :blur-radius}))))

(defmethod transform-tag :feMerge [tag svg]
  (let [[_ _ attrs _] (svg/tag-parts tag)]
    {:box-shadow true}))

(defmethod transform-tag :feColorMatrix [tag svg]
  (let [[_ _ attrs _] (svg/tag-parts tag)]
    (-> attrs
        (select-keys [:values])
        (update :values str->color-matrix)
        (clojure.set/rename-keys {:values :color-matrix}))))

(defn inject-attrs [tag attrs]
  (if (svg/tag? tag)
    (let [[_ id attrs2 body] (svg/tag-parts tag)]
      (into [(first tag) (merge attrs attrs2)] body))
    tag))

(defmethod transform-tag :g [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        body              (update (vec body) 0 #(inject-attrs % (select-keys attrs [:filter])))
        body              (map #(transform-tag % svg) body)
        group-content     (into
                            [(add-class :div id)
                             {:style {:position  :relative
                                      :min-width "2000px"}}]
                            body)
        attrs             (dissoc attrs :filter)
        styles            (cond-> {}
                            (:transform attrs) (merge {:position :absolute} {:my-style "style"} (transform-to-pos attrs)))]/
       (if styles
         [:div {:style styles} group-content]
         group-content)))

(defmethod transform-tag :rect [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t                 (add-class :div id)
        style             (-> attrs
                              transform-filter
                              (merge (dissoc attrs :filter))
                              attrs->style)]
    (prn style)
    (into
      [t style]
      (->> body (map #(transform-tag % svg))))))

(defmethod transform-tag :circle [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        t                 (add-class :div id)]
    (into
      [t (attrs->style attrs)]
      (->> body (map #(transform-tag % svg))))))

(defmethod transform-tag :ellipse [tag svg]
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
        link-id           (fix-id (:xlink-href attrs))]
    (transform-tag
      (let [tag (svg/find-tag-by-id svg link-id)]
        (inject-attrs tag attrs))
      svg)))

(defmethod transform-tag :image [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        base-64 (:xlink-href attrs)
        file-path  (save-base64 id base-64)]
    [:img (merge
           {:src file-path}
           (transform-to-pos (select-keys attrs [:width :height]))
           (attrs->style (dissoc attrs :xlink-href)))]))

(defmethod transform-tag :path [tag svg]
  (let [[_ id attrs body] (svg/tag-parts tag)
        bounds (select-keys attrs [:width :height])
        {:keys [width height]} bounds
        file-path  (save-svg id (h/html
                                  [:svg
                                   (merge
                                     {:viewBox (str "0 0 " width " " height)
                                      :version "1.1"
                                      :xmlns "http://www.w3.org/2000/svg"
                                      :xmlns:xlink "http://www.w3.org/1999/xlink"}
                                     (transform-to-pos (select-keys attrs [:width :height])))
                                   (svg/find-tag svg :defs)
                                   tag]))]
    [:img (merge
            {:src file-path :style {:position :absolute}}
            (transform-to-pos bounds))]))

(defmethod transform-tag :defs [tag svg]
  (let [[_ _ _ body] (svg/tag-parts tag)]
    (doseq [tag body]
      (swap! defs assoc (svg/tag->id tag) tag)
      (transform-tag tag svg))))

(defmethod transform-tag :pattern [tag svg]
  (let [[_ id] (svg/tag-parts tag)
        file-path (save-svg id (h/html [:svg {:version "1.1"
                                              :width "100%"
                                              :height "100%"
                                              :xmlns "http://www.w3.org/2000/svg"
                                              :xmlns:xlink "http://www.w3.org/1999/xlink"}
                                        [:defs tag]
                                        [:rect {:width "100%"
                                                :height "100%"
                                                :fill (format "url(#%s)" id)}]]))]
    nil))

(defmethod transform-tag :mask [tag svg]) ;; div
(defmethod transform-tag :default [tag svg])

(defn- relative-div? [x]
  (when (vector? x)
   (let [[_ _ attrs _] (svg/tag-parts x)]
     (-> attrs :style :position (= :relative)))))

(defn- inline-divs [dom]
  (walk/postwalk
   (fn [x]
     (if (and (single-tag? x) (relative-div? x))
       (-> x body first)
       x))
   dom))

(defn add-bounds-to-relative [dom]
  (walk/postwalk
   (fn [x]
     (if (relative-div? x)
       (let [w (dom/get-max-images-width x)]
         (if (pos? w)
           (assoc-in x [1 :style :min-width] (str w "px"))
           x))
       x))
   dom))

(defn transform [svg & [config]]
  #_(when-let [img-dir (io/resource (str "public/" (:img config)))]
    (io/delete-file img-dir))
  (-> (transform-tag svg svg)
      inline-divs
      add-bounds-to-relative
      ;; group-svgs
      ;; add-flex-layout
      ;; optimize-texts
      ;; extract-styles

      ))

(comment

  (svg-to-html.svg.core/svg->cljs
   "resources/svg/test-2.svg"
   "src/cljs/svg_to_html/test_dom.cljs"
   "svg-to-html.test-dom")




  )
