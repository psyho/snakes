(ns telnet-test
  (use midje.sweet
       telnet))

(defn string-input-stream [s]
  (java.io.ByteArrayInputStream. (.getBytes s)))

(fact "creating a lazy sequence from an input stream"
  (map char (make-input-stream (string-input-stream "foo"))) => [\f \o \o])
