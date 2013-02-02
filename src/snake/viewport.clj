(ns snake.viewport)

(declare stringify in-viewport?)

(defrecord Viewport [position rows cols chars]
  Object
  (toString [self] (stringify self)))

(defn viewport [[c r :as center] [cols rows :as size]]
  "creates a new viewport of given size, centerd at given position"
  (Viewport. [(- c (quot cols 2)) (- r (quot rows 2))]
             rows
             cols
             (vec (repeat rows (vec (repeat cols \.))))))

(defn vput [viewport [c r :as pos] chr]
  "returns a new viewport with chr renderd at pos"
  (if (in-viewport? viewport pos)
    (let [chars (:chars viewport)]
      (assoc viewport :chars (assoc-in chars (map - [r c] (reverse (:position viewport))) chr))) 
    viewport))

(defn- stringify [viewport]
  (clojure.string/join "\n\r" (map #(apply str %) (:chars viewport))))

(defn in-viewport? [{:keys [rows cols position]} [c r]]
  (let [[left top] position]
    (cond 
      (< c left) false
      (< r top) false
      (>= r (+ top rows)) false
      (>= c (+ left cols)) false
      :else true)))
