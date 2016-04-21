(ns episodic.log
  "Opinionated logging library"
  (:require [clojure.pprint :as pp]
            [clojure.stacktrace :as cs])
  (:import [java.util.concurrent
            ThreadFactory
            ThreadPoolExecutor
            ThreadPoolExecutor$DiscardPolicy
            ArrayBlockingQueue]))

(defn- inst-str [d]
  (let [s (-> d pp/pprint with-out-str)]
    (.substring s 0 (dec (.length s)))))

(defn episode-writer
  "Returns the java.io.Writer that the default implementation of print-episode
  writes to. The default implementation always returns *out*."
  []
  *out*)

(defn print-episode
  "Pretty-prints an episode map to (episode-writer). Prints an empty line before
  and after each episode.

  Could be replaced by prn for speed, or to avoid multi-line output."
  [m]
  (binding [*out* (episode-writer)]
    (let [v #(str % \space (% m))
          dv #(str % \space (inst-str (% m)))
          pp (fn [t]
               (when-let [v (t m)]
                 (println t)
                 (pp/pprint v)))]
      (println (str \newline  "{" (v :tag)) (dv :start))
      (pp :summary)
      (pp :error)
      (println :options (update (:options m) :rethrow #(if (fn? %) :fn %)))
      (println (v :thread-id) (v :sec) (str (dv :end) "}") \newline)
      (.flush *out*))))

(def print-pool
  "The ThreadPoolExecutor that print-episode calls are scheduled on (by the
  episode macro). The out of the box executor uses a single low-priority daemon
  thread, and discards tasks if tasks are scheduled faster than the system can
  handle."
  (ThreadPoolExecutor. 0 1
                       5 java.util.concurrent.TimeUnit/MINUTES
                       (ArrayBlockingQueue. 8)
                       (reify ThreadFactory
                         (newThread [_ runnable]
                                    (doto (Thread. runnable)
                                      (.setName (str ::print-thread))
                                      (.setPriority Thread/MIN_PRIORITY)
                                      (.setDaemon true))))
                       (ThreadPoolExecutor$DiscardPolicy.)))

(defn merge-fn
  "The merge function that summarize passes to merge-with. By default, if x is
  a collection then (into x y) is returned. Othewise, if both arguments are
  equal then the first is kept. Throws an exception in all other cases."
  [x y]
  (cond (coll? x) (into x y)
        (= x y) x
        :else (throw (IllegalArgumentException. (str "(not= " y " " x ")")))))

(defn summarize
  "The reducer that is used by the episode macro to compile notes into a single
  value, which is then passed to print-episode. The default implementation
  creates a map using (partial merge-with merge-fn) and indicates merge failures
  (exceptions) inline with the ::failed-to-merge keyword."
  ([] {})
  ([acc el] (try
              (merge-with merge-fn acc el)
              (catch IllegalArgumentException e
                (merge-with into acc {::failed-to-merge [el]})))))

(def global-options
  "Default options for episodes. These take the lowest priority."
  {:rethrow true
   :log-level-normal 1
   :log-level-error 9})

(def tag-options
  "Overwrite options of episodes with specific tags. These are higher priority
  than global-options but lower priority than options that are directly passed
  to the episode macro."
  {:debug-this {:log-level-normal 9}})

(def ^{:dynamic true :doc "Used by the episode macro. Not part of the public API."}
  *notes*)

(defmacro or-some
  "Used by the episode macro. Not part of the public API."
  ([] nil)
  ([x & next] `(if-some [x# ~x] x# (or-some ~@next))))

(defmacro episode
  "Start a new episode in which notes (see note) and post-its (see post) can be
  logged for varying log levels. At the end of the episode, a summary is
  compiled using summarize and printed using print-episode.

  The first vector is a mandatory tag (to identify the kind of episode) and an
  optional map of options. If an option is omitted here then tag-options or
  global-options is consulted.

  Options:
  - :log-level-normal is the log level for when no exception is thrown inside
    the episode
  - :log-level-error is the log level when an exception is thrown
  - :retrow - if false, exceptions are contained withing the episode. If true
    then the exception is rethrown (after wrapping with ex-info). The value for
    :rethrow can also be a unary function that is called with the summary if an
    exception is thrown."
  [[tag {:as opts}] & body]
  (let [opt-dict-name (gensym 'opts)
        all-opts (into {} (for [k [:rethrow :log-level-normal :log-level-error]]
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
        opt #(doto (-> all-opts % first) assert)]
    `(binding [*notes* (-> [] transient volatile!)]
       (let [~opt-dict-name ~opts
             ~@(norm-opts)
             start-time# (java.util.Date.)
             start-nanos# (System/nanoTime)
             episode-map# (promise)
             compile# (fn [error#]
                        (let [log# *notes*
                              end-nanos# (System/nanoTime)
                              thread-id# (.getId (Thread/currentThread))]
                          (delay {:tag ~tag
                                  :thread-id thread-id#
                                  :options ~opt-dict-name
                                  :start start-time#
                                  :sec (-> end-nanos# (- start-nanos#) (/ 1e9) (max 0.001))
                                  :end (java.util.Date.)
                                  :summary (->> @log#
                                                persistent!
                                                (mapcat (partial take (if error#
                                                                        ~(opt :log-level-error)
                                                                        ~(opt :log-level-normal))))
                                                (reduce summarize))
                                  :error (some-> error# Throwable->map)})))]
         (try
           ~@body
           (catch Throwable t#
             (deliver episode-map# (compile# t#))
             (when-let [rt# ~(opt :rethrow)]
               (if (fn? rt#)
                 (rt# @@episode-map#)
                 (throw (ex-info (str "rethrow in " ~tag)
                                 {:episode @@episode-map#}
                                 t#)))))
           (finally
             (deliver episode-map# (compile# nil))
             (.execute print-pool #(print-episode @@episode-map#))))))))

(defn note
  "Write a sequence of notes to the log. Includes the nth argument in the summary
  iff n is less than or equal to the applicable log level."
  [& notes]
  (vswap! *notes* conj! notes)
  nil)

(defn post [k & post-its]
  "Writes a key (of any type) and a sequence of post-its (of any type) to the
  log. The nth post-it is included in the final summary iff n is <= the
  applicable log level.

  Equivalent to (note {k [post-it-1]} ... {k [post-it-n]})."
  (apply note (for [x post-its] {k [x]})))