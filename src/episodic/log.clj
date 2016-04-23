(ns episodic.log
  "Opinionated logging library"
  (:require [episodic.log.options :as opts]))

(def ^{:private true :dynamic true} *log-level*)

(defn set-log-level
  "Reset the log level for the current episode, provided it hasn't been
  compiled yet."
  [n]
  (vreset! *log-level* n)
  nil)

(def ^{:dynamic true :private true} *log*)

(defn note
  "Write a sequence of notes to the log. Includes the nth argument in the
  summary iff n is less than or equal to the applicable log level."
  [& notes]
  (*log* notes)
  nil)

(defn post [k & post-its]
  "Writes a key (of any type) and a sequence of post-its (of any type) to the
  log. The nth post-it is included in the final summary iff n is <= the
  applicable log level.

  Equivalent to (note {k [post-it-1]} ... {k [post-it-n]})."
  (apply note (for [x post-its] {k [x]})))

(def ^:dynamic *global-options*
  "Default options for episodes. These take the lowest priority."
  {:default-log-level 1
   :logger opts/volatile-logger
   :catch (fn [e _] (set-log-level 9))
   :executor opts/lossy-executor
   :transducer identity
   :reducer opts/merge-into
   :transformer opts/default-transformer
   :get-writer (constantly *out*)
   :print opts/pretty-print})

(def ^:dynamic *tag-options*
  "Overwrite options of episodes with specific tags. These are higher priority
  than *global-options* but lower priority than options that are directly passed
  to the episode macro."
  {:debug-this {:default-log-level 9}})

(defmacro or-some
  "Used by the episode macro. Not part of the public API."
  ([] nil)
  ([x & next] `(if-some [x# ~x] x# (or-some ~@next))))

(defmacro episode
  "Start a new episode in which notes (see note) and post-its (see post) can be
  logged for varying log levels. At the end of the episode, a log level is
  selected depending on whether or not an exception was thrown. The notes and
  post-its are then compiled into a single map—the summary.

  The first vector is a mandatory tag (to identify the kind of episode) and an
  optional map of options. If an option is omitted here then *tag-options* or
  *global-options* is consulted.

  Options:
  - :catch Error handler, with a Throwable and a delay of the summary for args
  - :default-log-level is the initial log level
  - :executor ThreadPoolExecutor for writing the summary
  - :transducer Opportunity to rewrite notes before they are summarized
  - :reducer Reducer for compiling the summary
  - :transformer Merge context, the result of reducer, and the episode exception
  - :get-writer Function from summaries to a java.io.Writer object
  - :print Function that, given a summary, prints to *out*

  :catch is called when a Throwable is thrown in an episode. Typically the
  log level is changed at this point. :catch also allows for rethrowing the
  exception or determining the return value."
  [[tag {:as opts}] & body]
  (let [opt-dict-name (gensym 'opts)
        all-opts (into {} (for [k (keys *global-options*)]
                            [k [(-> k name gensym) k]]))
        norm-opts (fn []
                    (-> (mapcat (fn [[s k]]
                                  [s `(or-some (~k (get *tag-options* ~tag))
                                               (~k ~opt-dict-name)
                                               (~k *global-options*))])
                                (vals all-opts))
                        (concat [opt-dict-name
                                 (into {} (for [[s k _ _] (vals all-opts)]
                                            [k s]))])
                        vec))
        opt #(doto (-> all-opts % first) (assert (str "No option " %)))]
    `(let [~opt-dict-name ~opts
           ~@(norm-opts)
           log# (~(opt :logger))
           log-level# (volatile! ~(opt :default-log-level))
           commit-tr-context# (~(opt :transformer) ~tag)
           summary# (volatile! nil)
           commit# (fn [error#]
                     (let [tr# (commit-tr-context# error#)]
                       (->> (transduce (comp (mapcat (partial take @log-level#))
                                             ~(opt :transducer))
                                       ~(opt :reducer)
                                       (log#))
                            tr#
                            delay
                            (vreset! summary#))))]
       (binding [*log* log#
                 *log-level* log-level#]
         (try
           (let [retval# (do ~@body)]
             (commit# nil)
             retval#)
           (catch Throwable t#
             (commit# t#)
             (~(opt :catch) t# @summary#))
           (finally
             (.execute ~(opt :executor)
                       #(when-some [writer# (some-> @@summary#
                                                    (~(opt :get-writer)))]
                                   (binding [*out* writer# *err* writer#]
                                     (~(opt :print) @@summary#))))))))))