(ns episodic.core
  "Dynamic and composable logging library"
  (:require [episodic.impl :as impl]
            [episodic.impl.default :as default]))

(def set-log-level impl/set-log-level)

(def note impl/note)

(def ^:dynamic *options*
  "Options:
  - :logger The functions to write notes with
  - :runner Function that runs the body of the episode
  - :transducer Opportunity to rewrite notes before they are summarized
  - :reducer Reducer for compiling the summary"
  default/options)

(defmacro episode
  "Start a new episode in which notes (see note) can be logged for varying log
  levels. During the episode the log-level can be set (e.g. after an exception
  is thrown, the log level can be increased. At the end of the episode, a
  summary of notes can be compiled using the log level set at that point.

  The first vector is a mandatory tag (to identify the kind of episode) and an
  optional inline map of options to override in *options*."
  [[tag & {:as opts}] & body]
  `(let [opts# (into *options* ~opts)
         log# ((opts# :logger))
         log-level# (volatile! 1)]
     (binding [*options* opts#
               impl/*tag* ~tag
               impl/*log* log#
               impl/*log-level* log-level#
               impl/*summary* (delay (transduce (comp (mapcat (partial take @log-level#))
                                                      (opts# :transducer))
                                                (opts# :reducer)
                                                (log#)))]
       @((opts# :runner) (delay ~@body)))))