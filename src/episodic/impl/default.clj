(ns episodic.impl.default
  (:require [episodic.impl :refer :all]
            [clojure.pprint :as pp])
  (:import [java.util.concurrent
            ThreadFactory
            ThreadPoolExecutor
            ThreadPoolExecutor$DiscardPolicy
            ArrayBlockingQueue]))

(def lossy-executor
  "This ThreadPoolExecutor uses a single low-priority daemon thread, and
  discards tasks if tasks are scheduled faster than the system can handle."
  (ThreadPoolExecutor. 0 1
                       5 java.util.concurrent.TimeUnit/MINUTES
                       (ArrayBlockingQueue. 8)
                       (reify ThreadFactory
                         (newThread [_ runnable]
                                    (doto (Thread. runnable)
                                      (.setName (str ::lossy-executor))
                                      (.setPriority Thread/MIN_PRIORITY)
                                      (.setDaemon true))))
                       (ThreadPoolExecutor$DiscardPolicy.)))

(defn merge-into
  "Merges maps using Clojure's merge-with. Non-maps are merged as follows:
  - Collections are merged using into
  - For other identical values, only one is kept
  - Otherwise a merge error is noted inline with the ::failed-to-merge keyword."
  ([] {})
  ([x] x)
  ([x y] (try (cond (map? x) (merge-with merge-into x y)
                    (coll? x) (into x y)
                    (= x y) x
                    :else (throw (IllegalArgumentException.)))
           (catch IllegalArgumentException _
             {::failed-to-merge [{x y}]}))))

(def namespaced
  (map (fn [m]
         (mapcat (fn [[k v]]
                   (if (and (keyword? k) (namespace k))
                     {(-> k namespace keyword) {(-> k name keyword) v}}
                     {:other {k v}}))
                 m))))

(defn pretty-print [m]
  (println)
  (pp/pprint m)
  (println))

(defrunner run-with-context [d] [start-time (java.util.Date.)
                                 start-nanos (System/nanoTime)
                                 tag *tag*
                                 thread-id (.getId (Thread/currentThread))]
  @d
  (catch Throwable t
    (note {::error (Throwable->map t)})
    (set-log-level 9)
    (throw t))
  (finally (note {::tag tag
                  ::thread-id thread-id
                  ::start start-time
                  ::end (java.util.Date.)
                  ::millis (/ (- (System/nanoTime) start-nanos) 1e6)})))

(defrunner run-without-throwing [d] []
  @d
  (catch Throwable _))

(defrunner run-and-print [d] [summary *summary*]
  @d
  (finally (pretty-print @summary)))

(def options {:logger unsafe-logger
              :runner (comp (shadow-runner lossy-executor
                                           run-and-print)
                            run-without-throwing
                            run-with-context)
              :transducer namespaced
              :reducer merge-into})