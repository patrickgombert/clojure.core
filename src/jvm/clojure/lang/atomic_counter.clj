(ns clojure.lang.atomic-counter
  (:refer-clojure :only [defmacro])
  (:import [java.util.concurrent.atomic AtomicInteger]))

(defmacro get-and-increment-atomic-counter [counter]
  `(.getAndIncrement ~counter))

(defmacro get-atomic-counter [counter]
  `(.get ~counter))

(defmacro set-atomic-counter [counter v]
  `(.set ~counter ~v))

(defmacro new-atomic-counter
  ([] `(AtomicIntger. 1))
  ([value] `(AtomicInteger. ~value)))
