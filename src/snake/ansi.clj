(ns snake.ansi)

(def e "\u001B")

(def clear (str e "[2J"))
(def home (str e "[0;0f"))

(def green (str e "[32m"))
(def red (str e "[31m"))
(def reset (str e "[0m"))
