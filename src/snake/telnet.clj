(ns snake.telnet
  (:require [server.socket :as tcp]))

(defn ubyte [n]
  (byte (if (> n 127) (- n 256) n)))

(def naws (ubyte 31))
(def echo (ubyte 1))
(def will-suppress-go-ahead (ubyte 3))

(def sb   (ubyte 250))
(def will (ubyte 251))
(def wont (ubyte 252))
(def do   (ubyte 253))
(def dont (ubyte 254))
(def iac  (ubyte 255))

(defn to-unsigned [n]
  (bit-and n 0xFF))

(defn read-one-byte [stream]
  (try 
    (.readByte stream) 
    (catch java.io.EOFException e nil)
    (catch java.net.SocketException e nil)))

(defn make-input-stream [in]
 (let [stream (java.io.DataInputStream. in)]
   (take-while (complement nil?) (repeatedly #(read-one-byte stream)))))

(defn write-bytes [term c]
  (.write (:out @term) (byte-array c)))

(defn tprint [term s]
  (write-bytes term (.getBytes s)))

(defn drop-n-from-in [term n]
  (assoc term :in (drop n (:in term))))

(defn disregard-n-bytes [term n]
  (swap! term drop-n-from-in n))

(defn disable-echo [term]
  (write-bytes term [iac will echo])
  (disregard-n-bytes term 3))

(defn supress-go-ahead [term]
  (write-bytes term [iac will will-suppress-go-ahead])
  (disregard-n-bytes term 3))

(defn read-n-bytes [term n]
  (let [bytes (take n (:in @term))]
    (disregard-n-bytes term n)
    bytes))

(defn get-size [term]
  (let [_ (read-n-bytes term 3)
        [ch cl rh rl] (map to-unsigned (read-n-bytes term 4))
        _ (read-n-bytes term 2)
        word #(+ %2 (* 256 %1))]
    [(word ch cl) (word rh rl)]))

(defn get-terminal-size [term]
  (write-bytes term [iac do naws])
  (when (= [iac will naws] (read-n-bytes term 3))
    (let [size (get-size term)]
      (swap! term assoc :size size))))

(defn append-input [term c]
  (assoc term :input (conj (:input term) c)))

(defn start-input-reader [term]
  (.start (Thread. 
            #(doseq [c (:in @term)] 
               (swap! term append-input c)))))

(defn server-handler [f]
  (fn [in out]
    (let [term (atom {:in (make-input-stream in) 
                      :out out 
                      :input []})]
      (disable-echo term)
      (supress-go-ahead term)
      (get-terminal-size term)
      (start-input-reader term)
      (if (:size @term)
        (f term)
        (tprint term "Your terminal does not support NAWS. Bye.")))))
    
(defn start-telnet-server [port f]
  (tcp/create-server port (server-handler f)))
