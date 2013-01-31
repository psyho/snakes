(ns snake.core
  (require telnet 
           ansi
           [clojure.java.io :as io]))

(defn clear []
  (print (str ansi/clear ansi/home)))

(defn fix-newlines [s]
  (clojure.string/replace s "\n" "\n\r"))

(def logo (fix-newlines (slurp "src/logo.txt")))

(defmulti process-input (fn [player c] (:state player)))

(defmethod process-input :welcome [player c]
  (cond
    (= \return c) (assoc player :state :bye)
    (re-find #"[\w-+\.]" (str c)) (assoc player :name (str (:name player) c))
    :else player))

(defmethod process-input :bye [player c]
  player)

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

(defmethod render :bye [player game]
  (clear)
  (print (str "Bye, " (:name player) "!\n\r")))

(defn game-handler [term]
  (let [player (atom {:name "" :state :welcome})]
    (binding [*out* (io/writer (:out @term))]
      (loop [] 
        (dosync 
          (let [input (map char (:input @term))]
            (reset! player (reduce process-input @player input)) 
            (swap! term assoc :input [])))
        (render @player nil)
        (flush)
        (when (not= (:state @player) :bye) 
          (Thread/sleep 250)
          (recur))))))

(defn -main [& args]
  (telnet/start-telnet-server 6666 game-handler))
