(ns snake.core
  (:require [snake.telnet :as telnet] 
            [snake.ansi :as ansi]
            [snake.viewport :as v]
            [clojure.set :as sets]
            [clojure.java.io :as io]))

(defn clear []
  (print (str ansi/clear ansi/home)))

(defn fix-newlines [s]
  (clojure.string/replace s "\n" "\n\r"))

(def logo (fix-newlines (slurp "src/logo.txt")))

(def uuid (atom 0))
(defn next-uuid [] (swap! uuid inc))

(declare make-snake)

(defn random-pos [{:keys [board-size]}]
  (map (comp int rand) board-size))

(defn state [game uid]
  (get-in game [:players uid :state]))

(defn set-state [game uid state]
  (assoc-in game [:players uid :state] state))

(defn add-objects [game objects]
  (let [old-objects (:objects game)
        with-uids (map #(vector (:uid %) %) objects)]
    (assoc game :objects (into old-objects with-uids))))

(defn add-player [game uid]
  (let [snake (make-snake (random-pos game) uid)]
    (-> game
        (set-state uid :in-game)
        (add-objects [snake]))))

(defn player-name [game uid]
  (get-in game [:players uid :name]))

(defmulti process-input (fn [game uid c] (state game uid)))

(defn append-to-player-name [game uid c]
  (assoc-in game [:players uid :name] (str (player-name game uid) c)))

(defmethod process-input :welcome [game uid c]
  (cond
    (= \return c)                 (add-player game uid)
    (re-find #"[\w-+\.]" (str c)) (append-to-player-name game uid c)
    :else                         game))

(defn opposite? [[x y] [a b]]
  (or (= x a) (= y b)))

(defn change-direction [game uid dir]
  (if (opposite? dir (get-in game [:objects uid :direction]))
    game
    (assoc-in game [:objects uid :direction] dir)))

(declare up down left right)

(defmethod process-input :in-game [game uid c]
  (case c
    \q (set-state game uid :bye)
    \h (change-direction game uid left)
    \j (change-direction game uid down)
    \k (change-direction game uid up)
    \l (change-direction game uid right)
    game))

(defmethod process-input :bye [game uid c]
  game)

(defmulti render state)

(defn green [s]
  (str ansi/green s ansi/reset))

(defn red [s]
  (str ansi/red s ansi/reset))

(defmethod render :welcome [game uid]
  (clear)
  (print "\n\r")
  (print (green logo))
  (print "\n\r")
  (print "\n\r")
  (print "Enter your name: \n\r")
  (print "\n\r")
  (print "> ")
  (print (player-name game uid)))

(defmethod render :bye [game uid]
  (clear)
  (print (str "Bye, " (player-name game uid) "!\n\r")))

(defn apply-input [game uid input]
  (reduce (fn [game c] (process-input game uid c)) game input))

(def left [-1 0])
(def right [1 0])
(def up [0 -1])
(def down [0 1])

(defn move [position direction]
  (map + position direction))

(defn make-snake [position id]
  {:uid id
   :type :snake
   :is #{:movable :renderable :collidable}
   :blocks (take 3 (iterate #(move % right) position))
   :direction left})

(defn make-apple [position id]
  {:uid id
   :is #{:renderable :collidable}
   :type :apple
   :blocks [position]})

(defmulti render-object (fn [viewport object] (:type object)))

(defn- render-blocks [viewport blocks symbol]
  (reduce (fn [viewport block] (v/vput viewport block symbol)) viewport blocks))

(defmethod render-object :snake [viewport {:keys [blocks]}]
  (render-blocks viewport blocks (green "O")))

(defmethod render-object :apple [viewport {:keys [blocks]}]
  (render-blocks viewport blocks (red "@")))

(defmulti move-object (fn [o] (:type o)))

(defmethod move-object :snake [{:keys [blocks direction] :as snake}]
  (let [[head & _ :as tail] (butlast blocks)
        blocks (cons (move head direction) tail)]
    (assoc snake :blocks blocks)))

(defn get-objects [{:keys [objects]} type]
  (filter #((:is %) type) (vals objects)))

(defn move-objects [{:keys [objects] :as game}]
  (let [movable (get-objects game :movable)
        moved (map move-object movable)]
    (add-objects game moved)))

(defn render-game [viewport game]
  (let [renderables (get-objects game :renderable)]
    (reduce render-object viewport renderables)))

(defn user-position [{:keys [objects]} uid]
  (first (:blocks (get objects uid))))

(defn screen-size [game uid]
  (get-in game [:players uid :screen-size]))

(defmethod render :in-game [game uid]
  (let [viewport (v/viewport (user-position game uid) (screen-size game uid))
        viewport (render-game viewport game)]
    (clear)
    (print (str viewport))))

(defn add-apples [{:keys [objects] :as game} n]
  (let [apples (take n (repeatedly #(make-apple (random-pos game) (next-uuid))))]
    (add-objects game apples)))

(defmulti collide (fn [a b] [(:type a) (:type b)]))

(defn shorten-snake [{:keys [blocks] :as snake}]
  (assoc snake :blocks (take 3 blocks)))

(defn lenghten-snake [{:keys [blocks] :as snake}]
  (let [last-block (last blocks)
        new-blocks (:blocks (move-object snake))]
    (assoc snake :blocks (concat new-blocks [last-block]))))

(defn disappear-apple [apple]
  (assoc apple :is #{}))

(defmethod collide [:snake :snake] [a b]
  (map shorten-snake [a b]))

(defmethod collide [:apple :apple] [a b]
  (map disappear-apple [a b]))

(defmethod collide [:snake :apple] [snake apple]
  [(lenghten-snake snake) (disappear-apple apple)])

(defmethod collide [:apple :snake] [apple snake]
  [(disappear-apple apple) (lenghten-snake snake)])

(defn colliding? [a b]
  (seq (sets/intersection (set (:blocks a)) (set (:blocks b)))))

(defn collide-objects [{:keys [objects] :as game}]
  (let [collidable (get-objects game :collidable)
        after-collisions (apply concat 
                                (for [x collidable 
                                      y collidable
                                      :when (and (not= x y) (colliding? x y))]
                                  (collide x y)))]
    (add-objects game after-collisions)))

(defn make-game [board-size apple-count]
  (-> {:objects {} 
       :board-size board-size
       :players {}}
      (add-apples apple-count)))

(defn make-player [uid screen-size]
  {:name "" 
   :state :welcome 
   :screen-size screen-size
   :uid uid})

(def game (atom (make-game [100 100] 50)))

(defn game-handler [term]
  (let [uid (next-uuid)
        player (make-player uid (:size @term))]
    (swap! game assoc-in [:players uid] player)
    (binding [*out* (io/writer (:out @term))]
      (loop [] 
        (dosync 
          (let [input (map (comp char telnet/to-unsigned) (:input @term))]
            (swap! game apply-input uid input)
            (swap! term assoc :input [])))
        (render @game uid)
        (flush)
        (when (not= (state @game uid) :bye) 
          (Thread/sleep 250)
          (recur))))))

(defn game-loop []
  (swap! game move-objects) 
  (swap! game collide-objects) 
  (Thread/sleep 200) 
  (recur))

(defn -main [& args]
  (.start (Thread. game-loop))
  (telnet/start-telnet-server 6666 game-handler))
