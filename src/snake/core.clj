(ns snake.core
  (require telnet 
           ansi
           [clojure.set :as sets]
           [clojure.java.io :as io]))

(defn clear []
  (print (str ansi/clear ansi/home)))

(defn fix-newlines [s]
  (clojure.string/replace s "\n" "\n\r"))

(def logo (fix-newlines (slurp "src/logo.txt")))

(def uuid (atom 0))
(defn next-uuid [] (swap! uuid inc))

(def board-size [100 100])

(declare make-snake)

(defn random-pos []
  (map (comp int rand) board-size))

(defn add-player [player game]
  (let [uid (next-uuid)
        snake (make-snake (random-pos) uid)]
    [(assoc player :uid uid :state :in-game)
     (assoc-in game [:objects uid] snake)]))

(defmulti process-input (fn [player game c] (:state player)))

(defmethod process-input :welcome [player game c]
  (cond
    (= \return c)                 (add-player player game)
    (= (char 13) c)               [(assoc player :state :bye) game]
    (re-find #"[\w-+\.]" (str c)) [(assoc player :name (str (:name player) c)) game]
    :else                         [player game]))

(defn opposite? [[x y] [a b]]
  (or (= x a) (= y b)))

(defn change-direction [{:keys [uid] :as player} game dir]
  (if (opposite? dir (get-in game [:objects uid :direction]))
    [player game]
    [player (assoc-in game [:objects uid :direction] dir)]))

(declare up down left right)

(defmethod process-input :in-game [player game c]
  (case c
    \q [(assoc player :state :bye) game]
    \h (change-direction player game left)
    \j (change-direction player game down)
    \k (change-direction player game up)
    \l (change-direction player game right)
    [player game]))

(defmethod process-input :bye [player game c]
  [player game])

(defmulti render (fn [player game] (:state player)))

(defn green [s]
  (str ansi/green s ansi/reset))

(defn red [s]
  (str ansi/red s ansi/reset))

(defmethod render :welcome [player game]
  (clear)
  (print "\n\r")
  (print (green logo))
  (print "\n\r")
  (print "\n\r")
  (print "Enter your name: \n\r")
  (print "\n\r")
  (print "> ")
  (print (:name player)))

(defmethod render :bye [player game]
  (clear)
  (print (str "Bye, " (:name player) "!\n\r")))

(defn apply-input [player game input]
  (reduce (fn [[player game] c] (process-input player game c)) [player game] input))

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

(defn make-viewport [[c r] [cols rows]]
  {:position [(- c (quot cols 2)) (- r (quot rows 2))]
   :rows rows
   :cols cols
   :chars (vec (repeat rows (vec (repeat cols \.))))})

(defn in-viewport? [{:keys [rows cols position]} [c r]]
  (let [[left top] position]
    (cond 
      (< c left) false
      (< r top) false
      (>= r (+ top rows)) false
      (>= c (+ left cols)) false
      :else true)))

(defn vput [viewport [c r :as pos] chr]
  (if (in-viewport? viewport pos)
    (let [chars (:chars viewport)]
      (assoc viewport :chars (assoc-in chars (map - [r c] (reverse (:position viewport))) chr))) 
    viewport))

(defn stringify-viewport [viewport]
  (clojure.string/join "\n\r" (map #(apply str %) (:chars viewport))))

(defmulti render-object (fn [viewport object] (:type object)))

(defmethod render-object :snake [viewport {:keys [blocks]}]
  (reduce #(vput %1 %2 (green "O")) viewport blocks))

(defmethod render-object :apple [viewport {:keys [blocks]}]
  (reduce #(vput %1 %2 (red "@")) viewport blocks))

(defmulti move-object (fn [o] (:type o)))

(defmethod move-object :snake [{:keys [blocks direction] :as snake}]
  (let [[head & _ :as tail] (butlast blocks)
        blocks (cons (move head direction) tail)]
    (assoc snake :blocks blocks)))

(defn get-objects [{:keys [objects]} type]
  (filter #((:is %) type) (vals objects)))

(defn move-objects [{:keys [objects] :as game}]
  (let [movable (get-objects game :movable)
        objects (into objects (map #(vector (:uid %) (move-object %)) movable))]
    (assoc game :objects objects)))

(defn render-game [viewport game]
  (let [renderables (get-objects game :renderable)]
    (reduce render-object viewport renderables)))

(defn user-position [{:keys [uid]} {:keys [objects]}]
  (first (:blocks (get objects uid))))

(defmethod render :in-game [player game]
  (let [viewport (make-viewport (user-position player game) (:screen-size player))
        viewport (render-game viewport game)]
    (clear)
    (print (stringify-viewport viewport))))

(defn add-apples [{:keys [objects] :as game} n]
  (let [apples (take n (repeatedly #(make-apple (random-pos) (next-uuid))))]
    (assoc game :objects (into objects (map #(vector (:uid %) %) apples)))))

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
    (assoc game :objects (into objects (map #(vector (:uid %) %) after-collisions)))))

(def game (atom (add-apples {:objects {}} 100)))

(defn game-handler [term]
  (let [player (atom {:name "" :state :welcome :screen-size (:size @term)})]
    (binding [*out* (io/writer (:out @term))]
      (loop [] 
        (dosync 
          (let [input (map (comp char telnet/to-unsigned) (:input @term))
                [new-player new-game] (apply-input @player @game input)]
            (reset! player new-player) 
            (reset! game new-game)
            (swap! term assoc :input [])))
        (render @player @game)
        (flush)
        (when (not= (:state @player) :bye) 
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
