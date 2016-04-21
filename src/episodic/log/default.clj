(ns episodic.log.default
  (:refer-clojure :exclude [merge filter print])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.stacktrace :as cs])
  (:import [java.util.concurrent
            ThreadFactory
            ThreadPoolExecutor
            ThreadPoolExecutor$DiscardPolicy
            ArrayBlockingQueue]))

(def executor
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

(defn merge
  "Merges maps using Clojure's merge-with. Non-maps are merged as follows:
  - Collections are merged using into
  - For other identical values, only one is kept
  - Otherwise a merge error is noted inline with the ::failed-to-merge keyword."
  ([] {})
  ([acc el] (try
              (merge-with #(cond (coll? %1) (into %1 %2)
                                 (= %1 %2) %1
                                 :else (throw (IllegalArgumentException.
                                                (str "(not= " %1 " " %2 ")"))))
                          acc
                          el)
              (catch IllegalArgumentException e
                (merge-with into acc {::failed-to-merge [el]})))))

(def filter "Keeps all summaries as-is" identity)

(def get-writer "Always returns *out*" (constantly *out*))

(defn print
  "Pretty-prints an episode summary. Prints an empty line before and after the
  summary. Could be replaced by prn for speed, or to avoid multi-line output."
  [m]
  (let [v #(str % \space (% m))
        dv #(str % \space (-> m % prn with-out-str str/trim-newline))
        pp (fn [t]
             (when-let [v (t m)]
               (println t)
               (pp/pprint v)))]
    (println (str \newline  "{" (v :tag)) (dv :start))
    (pp :notes)
    (pp :error)
    (println :options (update (:options m) :rethrow #(if (fn? %) :fn %)))
    (println (v :thread-id) (v :sec) (str (dv :end) "}") \newline)
    (.flush *out*)))