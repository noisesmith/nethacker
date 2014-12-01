(ns org.noisesmith.nethacker
  (:require [schema.core :as schema])
  (:gen-class)
  (:import (java.util Date)
           (java.io StringReader PushbackReader)))

(def GameData
  "a schema for the state of a nethack game process interaction"
  {:name schema/Str
   :process java.lang.Process
   :in-buff bytes
   :err-buff bytes
   :in-bytes clojure.lang.IDeref ;; an int
   :err-bytes clojure.lang.IDeref ;; an int
   :debug schema/Bool
   :out [{}]})

(schema/defn write-input :- nil
  [game :- GameData
   s :- schema/Str]
  (doto (.getOutputStream (:process game))
    (.write (.getBytes s))
    (.flush))
  nil)

(schema/defn letter-command :- nil
  [game :- GameData
   c :- Character]
  (write-input game (str (int c) "\n")))

(schema/defn get-more :- clojure.lang.IDeref
  [source :- java.io.InputStream
   buff :- bytes]
  (future (.read source buff)))

(schema/defn handle-output :- GameData
  [buff :- bytes
   stream :- java.io.InputStream
   counter-future :- schema/Keyword
   output-type :- schema/Symbol]
  (schema/fn output-handler :- GameData
    [game :- GameData]
    (let [counter (get game counter-future)]
      (if (and (realized? counter)
               (>= @counter 0))
        (let [raw (subs (String. buff) 0 @counter)
              reader (try (-> raw
                              java.io.StringReader.
                              java.io.PushbackReader.)
                          (catch Exception _ false))
              forms (when reader
                      ((fn getd []
                         (when-let [form (try (read reader)
                                              (catch Exception e false))]
                           (cons form (getd))))))]
          (when (and (not-empty raw)
                     (:debug game))
            (println output-type \: raw))
          (assoc game
            counter-future (get-more stream buff)
            :out (conj (:out game) {:when (Date.)
                                    :type output-type
                                    :raw raw
                                    :data forms})))
        game))))

(schema/defn hack-loop :- GameData
  [game :- GameData]
  (let [{:keys [process in-buff err-buff debug]} game
        in-stream (.getInputStream process)
        err-stream (.getErrorStream process)
        handle-err (handle-output err-buff err-stream :err-bytes 'stderr)
        handle-in (handle-output in-buff in-stream :in-bytes 'stdout)]
    (-> game
        handle-err
        handle-in
        (#(if-not (.isAlive process)
             (do (println "game exited, code" (.exitValue process))
                 (assoc % :process nil
                        :return (.exitValue process)))
             %)))))

(schema/defn run-hack :- GameData
  [& [name :- schema/Str]]
  (let [name (or name "cloj")
        args ["/usr/games/nethack-lisp" "-u" name]
        pb (ProcessBuilder. args)
        process (.start pb)
        in-buff (byte-array 32768)
        err-buff (byte-array 32768)
        fut (doto (delay 0) force)]
    {:name name
     :process process
     :in-buff in-buff
     :err-buff err-buff
     :in-bytes fut
     :err-bytes fut
     :debug true
     :out []}))

(defn -main
  "runs a nethack game"
  [& [name :as args]]
  (run-hack name))
