(ns clojure.support.exception-assertions
  (:refer-clojure :only [defmacro list list*])
  (:require [clojure.next            :refer :all]
            [clojure.lang.exceptions :refer :all]))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(defmacro assertion-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? assertion-error msg body)))

(defmacro illegal-access-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? illegal-access-error msg body)))

(defmacro illegal-state-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? illegal-state-error msg body)))

(defmacro out-of-bounds-exception-is-thrown? [msg & body]
  (list 'is (list* 'thrown? out-of-bounds-exception msg body)))

(defmacro runtime-exception-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? runtime-exception msg body)))

