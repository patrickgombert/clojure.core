(ns clojure.lang.enumerable
  (:refer-clojure :only [defmacro deftype let reset! defn update-in fn cons list])
  (:require [clojure.next :refer [first next] :exclude [cons]]))

(def base-enumerator Iterable)

(deftype SeqIterator [^:unsynchronized-mutable -current-seq]
  java.util.Iterator
  (hasNext [this]
    (if -current-seq true false))

  (next [this]
    (if -current-seq
      (let [first-item (first -current-seq)]
        (set! -current-seq (next -current-seq))
        first-item)
      (throw (java.util.NoSuchElementException. ""))))

  (remove [this]
    (throw (UnsupportedOperationException. ""))))

(defn new-seq-iterator [-seq]
  (SeqIterator. -seq))

(defn has-more-elements? [^java.util.Enumeration iter]
  (.hasMoreElements iter))

(defn get-next [^java.util.Enumeration iter]
  (.nextElement iter))

(defmacro enumerable-method [bindings & body]
  `(iterator ~bindings ~@body))

