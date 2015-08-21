(ns clojure.lang.lock
  (:refer-clojure :only [defmacro])
  (:import [java.util.concurrent.locks ReentrantReadWriteLock]))

(defmacro new-read-write-lock []
  `(ReentrantReadWriteLock.))

(defmacro get-read-lock [rwlock]
  `(.readLock ~rwlock))

(defmacro get-write-lock [rwlock]
  `(.writeLock ~rwlock))

(defmacro lock [read-or-write-lock]
  `(.lock ~read-or-write-lock))

(defmacro unlock [read-or-write-lock]
  `(.unlock ~read-or-write-lock))
