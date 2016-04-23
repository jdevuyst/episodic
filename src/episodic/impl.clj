(ns episodic.impl
  (:import [java.util ArrayList]))

(def ^{:dynamic true :private true} *log-level*)

(def ^{:dynamic true :private true} *log*)

(def ^:dynamic *tag*)

(def ^:dynamic *summary*)

(defn set-log-level
  "Reset the log level for the current episode, provided it hasn't been
  compiled yet."
  [n]
  (vreset! *log-level* n)
  nil)

(defn note
  "Write a sequence of notes to the in-memory log. Includes the nth argument in
  the summary iff n is less than or equal to the applicable log level."
  [& notes]
  (*log* notes)
  nil)

(defmacro runner [[d] [& bindings] & body]
  `(fn [~d]
     (let [~@bindings]
       (delay (try ~@body)))))

(defmacro defrunner [name & xs]
  `(def ~name (runner ~@xs)))

(defn shadow-runner [executor f]
  (runner [d] [d2 (f d)]
          @d
          (finally (.execute executor
                             #(deref d2)))))

(defn unsafe-logger
  "Logs to a mutable list. Not thread-safe. May lead to problems when used in
  dosync, depending on the reducer."
  []
  (let [a (ArrayList.)]
    (fn
      ([] a)
      ([x] (.add a x)))))