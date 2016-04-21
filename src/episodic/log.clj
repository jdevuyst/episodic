(ns episodic.log
  "Opinionated logging library"
  (:require [episodic.log.default :as default]))

(def global-options
  "Default options for episodes. These take the lowest priority."
  {:log-level-normal 1
   :log-level-error 9
   :rethrow true
   :executor default/executor
   :merge default/merge
   :filter default/filter
   :get-writer default/get-writer
   :print default/print})

(def tag-options
  "Overwrite options of episodes with specific tags. These are higher priority
  than global-options but lower priority than options that are directly passed
  to the episode macro."
  {:debug-this {:log-level-normal 9}})

(def ^{:dynamic true :doc "Not part of the public API"}
  *log-level*)

(defn set-log-level
  "Reset the log level for the current episode, provided it hasn't been
  compiled yet."
  [n]
  (reset! *log-level* n))

(def ^{:dynamic true :doc "Not part of the public API"}
  *notes*)

(defn note
  "Write a sequence of notes to the log. Includes the nth argument in the
  summary iff n is less than or equal to the applicable log level."
  [& notes]
  (vswap! *notes* conj! notes)
  nil)

(defn post [k & post-its]
  "Writes a key (of any type) and a sequence of post-its (of any type) to the
  log. The nth post-it is included in the final summary iff n is <= the
  applicable log level.

  Equivalent to (note {k [post-it-1]} ... {k [post-it-n]})."
  (apply note (for [x post-its] {k [x]})))

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
  optional map of options. If an option is omitted here then tag-options or
  global-options is consulted.

  Options:
  - :retrow - false, true, or a function
  - :log-level-normal is the log level for when no exception is thrown
  - :log-level-error is the log level when an exception is thrown
  - :executor ThreadPoolExecutor for writing the summary
  - :merge Reducer for compiling the summary
  - :filter Function to cut parts of the summary for printing; return nil to skip printing
  - :get-writer Function from summaries to a java.io.Writer object
  - :print Function that, given a summary, prints to *out*

  If :rethrow is false then exceptions are contained within the episode. If true
  then the exception is rethrown (after wrapping with ex-info). The value for
  :rethrow can also be a unary function that is called with the summary if an
  exception is thrown."
  [[tag {:as opts}] & body]
  (let [opt-dict-name (gensym 'opts)
        all-opts (into {} (for [k (keys global-options)]
                            [k [(-> k name gensym) k]]))
        norm-opts (fn []
                    (-> (mapcat (fn [[s k]]
                                  [s `(or-some (~k (get tag-options ~tag))
                                               (~k ~opt-dict-name)
                                               (~k global-options))])
                                (vals all-opts))
                        (concat [opt-dict-name
                                 (into {} (for [[s k _ _] (vals all-opts)]
                                            [k s]))])
                        vec))
        opt #(doto (-> all-opts % first) (assert (str "No option " %)))]
    `(let [~opt-dict-name ~opts
           ~@(norm-opts)
           notes# (-> [] transient volatile!)
           log-level# (atom ~(opt :log-level-normal))
           start-time# (java.util.Date.)
           start-nanos# (System/nanoTime)
           episode-map# (promise)
           compile# (fn [error#]
                      (let [end-nanos# (System/nanoTime)
                            thread-id# (.getId (Thread/currentThread))]
                        (delay {:tag ~tag
                                :thread-id thread-id#
                                :options ~opt-dict-name
                                :start start-time#
                                :sec (-> end-nanos# (- start-nanos#) (/ 1e9) (max 0.001))
                                :end (java.util.Date.)
                                :notes (->> @notes#
                                            persistent!
                                            (mapcat (partial take @log-level#))
                                            (reduce ~(opt :merge)))
                                :error (some-> error# Throwable->map)})))]
       (binding [*notes* notes#
                 *log-level* log-level#]
         (try
           ~@body
           (catch Throwable t#
             (set-log-level ~(opt :log-level-error))
             (deliver episode-map# (compile# t#))
             (when-let [rt# ~(opt :rethrow)]
               (if (fn? rt#)
                 (rt# @@episode-map#)
                 (throw (ex-info (str "rethrow in " ~tag)
                                 {:episode @@episode-map#}
                                 t#)))))
           (finally
             (deliver episode-map# (compile# nil))
             (.execute ~(opt :executor)
                       #(when-let [m# (~(opt :filter) @@episode-map#)]
                          (binding [*out* (~(opt :get-writer) m#)
                                    *err* m#]
                            (~(opt :print) m#))))))))))