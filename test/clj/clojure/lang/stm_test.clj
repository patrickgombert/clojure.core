(ns clojure.lang.stm-test
  (:refer-clojure :only [defmacro fn let list list* pos?])
  (:require [clojure.test     :refer :all]
            [clojure.lang.stm :as stm]
            [clojure.next     :refer :all :exclude []]))
