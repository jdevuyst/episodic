(ns episodic.log
  (:require [clojure.pprint :as pp]
            [clojure.stacktrace :as cs])
  (:import [java.util.concurrent
            ThreadPoolExecutor
            ThreadPoolExecutor$DiscardPolicy
            ArrayBlockingQueue]))

(defn- inst-str [d]
  (let [s (-> d pp/pprint with-out-str)]
    (.substring s 0 (dec (.length s)))))

(defn print-episode [f]
  (let [m (f)
        v #(str % \space (% m))
        dv #(str % \space (inst-str (% m)))
        pp (fn [t]
             (when-let [v (t m)]
               (println t)
               (pp/pprint v)))]
    (println (str \newline  "{" (v :tag)) (v :arc) (dv :start))
    (pp :summary)
    (pp :error)
    (println :options (update (:options m) :rethrow #(if (fn? %) :fn %)))
    (println (v :thread-id) (v :sec) (str (dv :end) "}") \newline)
    (.flush *out*)))

(def print-pool (ThreadPoolExecutor. 0 1
                                     5 java.util.concurrent.TimeUnit/MINUTES
                                     (ArrayBlockingQueue. 8)
                                     (ThreadPoolExecutor$DiscardPolicy.)))

(defn merge-fn [x y]
  (cond (coll? x) (into x y)
        (= x y) x
        :else (throw (IllegalArgumentException. (str "(not= " y " " x ")")))))

(defn summarize
  ([] {})
  ([acc el] (try
              (merge-with merge-fn acc el)
              (catch IllegalArgumentException e
                (merge-with into acc {::failed-to-merge [el]})))))

(def global-options (atom {:rethrow true
                           :log-level-normal 1
                           :log-level-error 9}))

(def tag-options (atom {:debug-this {:log-level-normal 9}}))

(def ^:dynamic *episode*)

(defmacro or-some
  ([] nil)
  ([x & next] `(if-some [x# ~x] x# (or-some ~@next))))

(defmacro episode [[tag opts] & body]
  (let [opt-dict-name (gensym 'opts)
        all-opts (into {} (for [k [:rethrow :log-level-normal :log-level-error]]
                            [k [(-> k name gensym) k]]))
        norm-opts (fn []
                    (-> (mapcat (fn [[s k]]
                                  [s `(or-some (~k (get @tag-options ~tag))
                                               (~k ~opt-dict-name)
                                               (~k @global-options))])
                                (vals all-opts))
                        (concat [opt-dict-name
                                 (into {} (for [[s k _ _] (vals all-opts)]
                                            [k s]))])
                        vec))
        opt #(doto (-> all-opts % first) assert)]
    `(let [~opt-dict-name ~opts
           ~@(norm-opts)
           start-nanos# (System/nanoTime)
           close# (fn [m#] (assoc m#
                             :sec (-> (System/nanoTime) (- start-nanos#) (/ 1e9) (max 0.001))
                             :end (java.util.Date.)))]
       (binding [*episode* (volatile! {:tag ~tag
                                       :thread-id (.getId (Thread/currentThread))
                                       :options ~opt-dict-name
                                       :start (java.util.Date.)
                                       :arc (gensym ~tag)
                                       :log []})]
         (try
           ~@body
           (vswap! *episode* close#)
           (catch Throwable t#
             (vswap! *episode* close#)
             (->> t#
                  Throwable->map
                  (vswap! *episode* assoc :error))
             (when-let [rt# ~(opt :rethrow)]
               (if (fn? rt#)
                 (rt# @*episode*)
                 (throw t#))))
           (finally (-> (fn [m#]
                          (assoc m#
                            :summary (->> m#
                                          :log
                                          (mapcat (partial take (if (:error m#)
                                                                  ~(opt :log-level-error)
                                                                  ~(opt :log-level-normal))))
                                          (reduce summarize))))
                        (partial @*episode*)
                        (->> (partial print-episode)
                             (.execute print-pool)))))))))

(defn note [& xs]
  (vswap! *episode* update-in [:log] conj xs)
  nil)

(defn post [k & xs]
  (apply note (for [x xs] {k [x]})))

(defn cont [tag f]
  (let [ep @*episode*]
    (fn [& args]
      (episode [tag (:options ep)]
               (vswap! *episode* assoc :arc (:arc ep))
               (apply f args)))))