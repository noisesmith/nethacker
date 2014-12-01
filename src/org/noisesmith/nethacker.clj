(ns org.noisesmith.nethacker
  (:gen-class))

(defonce game (atom nil))

(defn write-input
  [s]
  (doto (.getOutputStream (:process @game))
    (.write (.getBytes s))
    (.flush))
  nil)

(defn letter-command
  [c]
  (write-input (str (int c) "\n")))

(defn get-more
  [source buff]
  (future (.read source buff)))

(defn hack-loop
  [] 
  (let [{:keys [process in-buff err-buff in-bytes err-bytes]} @game
        in-stream (.getInputStream process)
        err-stream (.getErrorStream process)
        out-stream (.getOutputStream process)
        next-in (delay (get-more in-stream in-buff))
        next-err (delay )]
    (when (realized? err-bytes)
      (when (> @err-bytes 0)
        (println "Error: " (subs (String. err-buff) 0 @err-bytes)))
      (swap! game assoc :err-bytes (get-more err-stream err-buff)))
    (when (realized? in-bytes)
      (when (> @in-bytes 0)
        ;; bind input, read, dispatch what is read
        (println "In: " (subs (String. in-buff) 0 @in-bytes)))
      (swap! game assoc :in-bytes (get-more in-stream in-buff)))
    (if-not (.isAlive process)
      (println "game exited"))))

(defn run-hack
  [& [name]]
  (let [pb (ProcessBuilder. ["/usr/games/nethack-lisp" "-u" (or name "cloj")])
        process (.start pb)
        in-buff (byte-array 32768)
        err-buff (byte-array 32768)
        fut (doto (delay 0) force)]
    (reset! game {:process process
                  :in-buff in-buff
                  :err-buff err-buff
                  :in-bytes fut
                  :err-bytes fut})))
(defn -main
  "runs a nethack game"
  [& [name :as args]]
  (run-hack name))
