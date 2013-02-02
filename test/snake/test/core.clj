(ns snake.test.core
  (:require [snake.viewport :as v])
  (:use midje.sweet
        snake.core))

(fact "snake rendering"
  (let [v (v/viewport [0 0] [5 3])
        snake (make-snake [0 0] 123)
        snake-str (apply str (repeat 3 (green "O")))]
    (str (render-object v snake)) => (str ".....\n\r.." snake-str "\n\r.....")))

(fact "apple rendering"
  (let [v (v/viewport [0 0] [5 3])
        apple (make-apple [0 0] 123)]
    (str (render-object v apple)) => (str ".....\n\r.." (red "@") "..\n\r.....")))
