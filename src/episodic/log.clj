(ns episodic.log
  (:require [clojure.pprint :as cp]
            [clojure.stacktrace :as cs]))

(defn- inst-str [d]
  (let [s (-> d cp/pprint with-out-str)]
    (.substring s 0 (dec (.length s)))))

(def print-sep ";;\n")

(def print-agent (agent nil))

(defn print-episode [m]
  (let [v #(str % \space (% m))
        dv #(str % \space (inst-str (% m)))
        pp (fn [t]
             (when-let [v (t m)]
               (print (str print-sep t \newline))
               (cp/pprint v)))]
    (println (str \newline  "{" (v :tag)) (dv :start))
    (pp :summary)
    (pp :error)
    (println (str print-sep (v :options)))
    (println (v :thread-id) (v :sec) (str (dv :end) "}") \newline)
    (.flush *out*)))

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

(def global-options (atom {:rethrow true}))

(def tag-options (atom {::blah {:log-level-normal 2}}))

(def ^{:dynamic true :private true} *episode-ref*)

(defmacro or-some
  ([] nil)
  ([x & next] `(if-some [x# ~x] x# (or-some ~@next))))

(defmacro episode [[moniker opts] & body]
  (let [tag (keyword (name (ns-name *ns*)) (str moniker))
        opt-dict-name (gensym 'opts)
        all-opts (into {} (for [[k t d] [[:rethrow? boolean true]
                                         [:log-level-normal int 1]
                                         [:log-level-error int 9]]]
                            [k [(-> k name gensym) k t d]]))
        norm-opts (fn []
                    (-> (mapcat (fn [[s k t d]]
                                  [s `(~t (or-some (~k ~opt-dict-name)
                                                   (~k (get @tag-options ~tag))
                                                   (~k @global-options)
                                                   ~d))])
                                (vals all-opts))
                        (concat [opt-dict-name
                                 (into {} (for [[s k _ _] (vals all-opts)]
                                            [k s]))])
                        vec))
        opt #(doto (-> all-opts % first) assert)]
    `(let [~opt-dict-name ~opts
           ~@(norm-opts)
           ~moniker (ref {:tag ~tag
                          :thread-id (.getId (Thread/currentThread))
                          :options ~opt-dict-name
                          :start (java.util.Date.)
                          :log []})
           start-nanos# (System/nanoTime)]
       (binding [*episode-ref* ~moniker]
         (try
           ~@body
           (catch Exception e#
             (-> e#
                 cs/root-cause
                 cp/pprint
                 with-out-str
                 str
                 (.substring 7)
                 read-string
                 (->> (alter ~moniker assoc :error))
                 dosync)
             (when ~(opt :rethrow?)
               (throw e#)))
           (finally (->> (fn [m#]
                           (assoc m#
                             :sec (-> (System/nanoTime) (- start-nanos#) (/ 1000000000) double)
                             :end (java.util.Date.)
                             :summary (->> m#
                                           :log
                                           (mapcat (partial take (if (:error m#)
                                                                   ~(opt :log-level-error)
                                                                   ~(opt :log-level-normal))))
                                           (reduce summarize))))
                         (alter ~moniker)
                         dosync
                         (send print-agent #(print-episode %2)))))))))

(defn note* [& xs]
  (alter *episode-ref* update-in [:log] conj xs)
  nil)

(defn note [& xs]
  (dosync (apply note* xs)))

(defn post* [k & xs]
  (apply note* (for [x xs] {k [x]})))

(defn post [k & xs]
  (dosync (apply post* k xs)))