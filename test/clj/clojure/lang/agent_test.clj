(ns clojure.lang.agent-test
  (:refer-clojure :only [class defmacro false? fn let list list* nil? true? while])
  (:require [clojure.lang.agent-test-def      :refer [gen-agent-test]]
            [clojure.test                     :refer :all]
            [clojure.next                     :refer :all]
            [clojure.lang.thread              :refer [sleep]]
            [clojure.lang.platform.exceptions :refer [argument-error new-runtime-exception runtime-exception]]))

(defmacro runtime-exception-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? runtime-exception msg body)))

(gen-agent-test)

