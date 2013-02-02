(ns snake.test.viewport
  (:use snake.viewport
        midje.sweet))

(fact "creating a viewport"
  (let [v (viewport [0 10] [50 90])]
    (:position v) => [-25 -35]
    (:cols v) => 50
    (:rows v) => 90
    (every? #(= \. %) (for [r (range 50) c (range 90)] (get-in (:chars v) [c r]))) => true))

(fact "rendering a viewport to a string"
  (let [v (viewport [0 0] [5 3])]
    (str v) => ".....\n\r.....\n\r....."))

(fact "viewport includes points based on it's position"
  (let [v (viewport [0 1] [3 5])]
    (every? true? (for [c [-1 0 1] r [-1 0 1 2 3]] (in-viewport? v [c r]))) => true
    (every? false? (for [c [-2 2] r [-1 0 1 2 3]] (in-viewport? v [c r]))) => true
    (every? false? (for [c [-1 0 1] r [-2 4]] (in-viewport? v [c r]))) => true))

(fact "rendering into viewport"
  (let [v (viewport [0 0] [5 3])]
    (str (vput v [0 0] "X")) => ".....\n\r..X..\n\r....."
    (str (vput v [-3 0] "X")) => ".....\n\r.....\n\r....."))
