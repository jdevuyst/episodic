(ns episodic.impl.default-test
  (:require [clojure.test :refer :all]
            [episodic.impl.default :as default]
            [episodic.impl :as impl]))

(deftest test-default-impl
  (testing "Merge into map"
    (is (= {} (default/merge-into)))
    (is (let [x (gensym)] (= x (default/merge-into x))))
    (is (= {:a {:b [1 2 2 1]}}
           (default/merge-into {:a {:b [1 2]}} {:a {:b [2 1]}})))
    (is (= {:a {:b #{1 2 3}}}
           (default/merge-into {:a {:b #{1 2}}} {:a {:b [2 3]}})))
    (is (= {:episodic.impl.default/failed-to-merge [{1 2}]}
           (default/merge-into 1 2)))
    (is (:episodic.impl.default/failed-to-merge (default/merge-into [2] 3)))
    (is (= {1 (default/merge-into [2] 3)}
           (default/merge-into {1 [2]} {1 3}))))
  (testing "Namespaced"
    (is (= [#{[:other {:a :b}]
              [:other {1 2}]
              [:episodic.impl.default-test {:a ::b}]}
            #{[:other {3 4}]}]
           (map set (transduce default/namespaced
                               conj
                               [{1 2 :a :b ::a ::b}
                                {3 4}])))))
  (testing "Runners"
    (is (not @(default/run-without-throwing (delay (throw (Error.))))))))