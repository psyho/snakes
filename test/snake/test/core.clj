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

(fact "on welcome screen input is appended to player name"
  (let [game {:players {123 {:name "" :state :welcome}}}
        processed (-> game
                      (process-input 123 \F)
                      (process-input 123 \o)
                      (process-input 123 \o))]
    (player-name processed 123) => "Foo"))

(fact "on welcome screen when player presses enter they are advaned to in-game screen"
  (let [game {:players {123 {:name "psyho" :state :welcome}}}
        game (process-input game 123 \return)]
    (state game 123) => :in-game))

(defn with-dir [dir]
  {:objects {123 {:direction dir}} :players {123 {:uid 123 :state :in-game}}})

(defn dir [game]
  (get-in game [:objects 123 :direction]))

(fact "in game pressing h changes snake's direction to left"
  (let [g (with-dir up)]
    (dir (process-input g 123 \h)) => left))

(fact "in game pressing l changes snake's direction to right"
  (let [g (with-dir up)]
    (dir (process-input g 123 \l)) => right))

(fact "in game pressing j changes snake's direction to down"
  (let [g (with-dir left)]
    (dir (process-input g 123 \j)) => down))

(fact "in game pressing k changes snake's direction to up"
  (let [g (with-dir right)]
    (dir (process-input g 123 \k)) => up))

(fact "snake can not change direction to the opposite"
  (let [g (with-dir up)]
    (dir (process-input g 123 \j)) => up)
  (let [g (with-dir down)]
    (dir (process-input g 123 \k)) => down)
  (let [g (with-dir left)]
    (dir (process-input g 123 \l)) => left)
  (let [g (with-dir right)]
    (dir (process-input g 123 \h)) => right))

