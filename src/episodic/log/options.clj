(ns episodic.log.options
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.stacktrace :as cs])
  (:import [java.util.concurrent
            ThreadFactory
            ThreadPoolExecutor
            ThreadPoolExecutor$DiscardPolicy
            ArrayBlockingQueue]))

(defn volatile-logger []
  (let [!v (-> [] transient volatile!)]
    (fn
      ([] (persistent! @!v))
      ([x] (vswap! !v conj! x)))))

(defn ref-logger []
  (let [r (ref [])]
    (fn
      ([] @r)
      ([x] (alter r conj x)))))

(def lossy-executor
  "This ThreadPoolExecutor uses a single low-priority daemon thread, and
  discards tasks if tasks are scheduled faster than the system can handle."
  (ThreadPoolExecutor. 0 1
                       5 java.util.concurrent.TimeUnit/MINUTES
                       (ArrayBlockingQueue. 8)
                       (reify ThreadFactory
                         (newThread [_ runnable]
                                    (doto (Thread. runnable)
                                      (.setName (str ::executor-thread))
                                      (.setPriority Thread/MIN_PRIORITY)
                                      (.setDaemon true))))
                       (ThreadPoolExecutor$DiscardPolicy.)))

(defn default-transformer [tag]
  (let [thread-id (.getId (Thread/currentThread))
        start-time (java.util.Date.)
        start-nanos (System/nanoTime)]
    (fn [e]
      (let [context {:tag tag
                     :thread-id thread-id
                     :start start-time
                     :end (java.util.Date.)
                     :millis (/ (- (System/nanoTime) start-nanos) 1e6)}]
        (fn [m]
          (cond-> {:notes m
                   :context context}
                  e (assoc :error (Throwable->map e))))))))

(defn merge-into
  "Merges maps using Clojure's merge-with. Non-maps are merged as follows:
  - Collections are merged using into
  - For other identical values, only one is kept
  - Otherwise a merge error is noted inline with the ::failed-to-merge keyword."
  ([] {})
  ([x] x)
  ([acc el] (try
              (merge-with #(cond (coll? %1) (into %1 %2)
                                 (= %1 %2) %1
                                 :else (throw (IllegalArgumentException.
                                                (str "(not= " %1 " " %2 ")"))))
                          acc
                          el)
              (catch IllegalArgumentException e
                (merge-with into acc {::failed-to-merge [el]})))))

(defn pretty-print [m]
  (println)
  (pp/pprint m)
  (println))