(ns episodic.core-test
  (:require [clojure.test :refer :all]
            [episodic.core :as ep]
            [episodic.impl :as impl]
            [episodic.impl.default :as default]))

(deftest test-episode-macro
  (binding [ep/*options* (assoc ep/*options* :runner identity)]
    (testing "Return values and eror catching"
      (is (= :retval
             (ep/episode [::foo]
                         :retval)))
      (is (= :constval
             (binding [ep/*options* (assoc ep/*options*
                                      :runner (fn [_] (delay :constval)))]
               (ep/episode [:foo]
                           :retval))
             (ep/episode [:foo :runner (fn [_] (delay :constval))]
                         :retval)
             (ep/episode [:foo :runner (fn [_] (delay :constval))]
                         (throw (NegativeArraySizeException.)))))
      (is (= :exception(try (ep/episode [::foo]
                                        (throw (NegativeArraySizeException.))
                                        :retval)
                         (catch NegativeArraySizeException _
                           :exception))))
      (is (= nil
             (ep/episode [::foo :runner default/run-without-throwing]
                         (throw (Error.))
                         :retval)))))
  (testing "Summary"
    (binding [ep/*options* (assoc ep/*options*
                             :runner (fn [d] @d impl/*summary*))]
      (is (= [1 2 3 4]
             (ep/episode [:foo
                          :transducer identity
                          :reducer conj]
                         (ep/set-log-level 2)
                         (ep/note 1 2)
                         (ep/note 3 4 5))))
      (is (= 15
             (ep/episode [:foo
                          :transducer (comp (filter even?)
                                            (map inc))
                          :reducer +]
                         (ep/set-log-level 3)
                         (ep/note 1 2 3)
                         (ep/note 4 5 6 7)))))))