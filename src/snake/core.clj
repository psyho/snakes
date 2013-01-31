(ns snake.core
  (require telnet 
           ansi
           [clojure.java.io :as io]))

(defn clear []
  (print (str ansi/clear ansi/home)))

(defn fix-newlines [s]
  (clojure.string/replace s "\n" "\n\r"))

(def logo (fix-newlines (slurp "src/logo.txt")))

(defmulti process-input (fn [player game c] (:state player)))

(defmethod process-input :welcome [player game c]
  (cond
    (= \return c)                 [(assoc player :state :in-game) game]
    (= (char 13) c)               [(assoc player :state :bye) game]
    (re-find #"[\w-+\.]" (str c)) [(assoc player :name (str (:name player) c)) game]
    :else                         [player game]))

(defmethod process-input :in-game [player game c]
  [player game])

(defmethod process-input :bye [player game c]
  [player game])

(defmulti render (fn [player game] (:state player)))

(defmethod render :welcome [player game]
  (clear)
  (print "\n\r")
  (print ansi/green)
  (print logo)
  (print ansi/reset)
  (print "\n\r")
  (print "\n\r")
  (print "Enter your name: \n\r")
  (print "\n\r")
  (print "> ")
  (print (:name player)))

(defmethod render :in-game [player game]
  (clear)
  (print "in game\n\r"))

(defmethod render :bye [player game]
  (clear)
  (print (str "Bye, " (:name player) "!\n\r")))

(defn apply-input [player game input]
  (reduce (fn [[player game] c] (process-input player game c)) [player game] input))

(def game (atom {:players [] :objects []}))

(defn game-handler [term]
  (let [player (atom {:name "" :state :welcome})]
    (binding [*out* (io/writer (:out @term))]
      (loop [] 
        (dosync 
          (let [input (map char (:input @term))
                [new-player new-game] (apply-input @player @game input)]
            (reset! player new-player) 
            (reset! game new-game)
            (swap! term assoc :input [])))
        (render @player nil)
        (flush)
        (when (not= (:state @player) :bye) 
          (Thread/sleep 250)
          (recur))))))

(defn -main [& args]
  (telnet/start-telnet-server 6666 game-handler))
