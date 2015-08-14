(ns clojure.lang.stm-test
  (:refer-clojure :only [defmacro fn let list list* pos?])
  (:require [clojure.test     :refer :all]
            [clojure.lang.stm :as stm]
            [clojure.next     :refer :all :exclude []]))

(deftest stm-test
  (testing "creates a new transaction if one is not running"
    (let [transaction (atom nil)]
      (is (nil? stm/*transaction*))
      (stm/run-in-transaction (fn [] (reset! transaction stm/*transaction*)))
      (is (= {} (deref transaction))))))

