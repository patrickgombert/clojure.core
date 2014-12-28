(ns org.clojure.lang.agent-test
  (:require [clojure.lang.agent-test-def :refer [gen-agent-test]]
            [clojure.test                :refer :all]))

(def argument-error IllegalArgumentException)

(def runtime-exception RuntimeException)

(defmacro new-runtime-exception [& args]
  (list* 'new 'RuntimeException args))

(defmacro runtime-exception-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? 'RuntimeException msg body)))

(defmacro sleep [t]
  `(Thread/sleep ~t))

(gen-agent-test)

