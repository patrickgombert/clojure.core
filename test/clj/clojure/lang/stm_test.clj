(ns clojure.lang.stm-test
  (:refer-clojure :only [let loop])
  (:require [clojure.test        :refer :all]
            [clojure.lang.stm    :as    stm]
            [clojure.lang.thread :refer [sleep]]
            [clojure.next        :refer :all]))

(deftest get-running-test
  (testing "set and get the running value"
    ; This is a clever way to capture the running transaction but
    ; I can't think of another way to write this test because
    ; of the ThreadLocal constraint
    (stm/run-in-transaction
      #(is (= 0 (.status (stm/get-info (stm/get-running)))))))

  )

