(ns clojure.lang.meta
  (:refer-clojure :only [defn])
  (:require [clojure.lang.platform.meta]
            [clojure.lang.protocols :refer [-meta -with-meta -reset-meta! -alter-meta!]]))

(defn meta [this]
  (-meta this))

(defn with-meta [this new-meta]
  (-with-meta this new-meta))

(defn reset-meta! [this new-meta]
  (-reset-meta! this new-meta))

(defn alter-meta! [this f & args]
  (-alter-meta! this f args))
