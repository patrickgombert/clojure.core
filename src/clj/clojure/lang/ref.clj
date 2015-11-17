(ns clojure.lang.ref
  (:refer-clojure :only [cons defn defn- defprotocol deftype if-not let apply])
  (:require [clojure.lang
              [atomic-counter :refer [new-atomic-counter]]
              [atomic-ref     :refer [new-atomic-ref ref-set!
                                      ref-get ref-compare-and-set!]]
              [exceptions     :refer [new-illegal-state-error]]
              [lock           :refer [lock unlock
                                      get-read-lock new-read-write-lock]]
              [protocols      :refer [IComparable IDeref IMeta IReference
                                      IValidatable IWatchable]]
              [stm            :as stm]]
            [clojure.next     :refer :all :exclude [cons]]))

(def ^:private MIN-HISTORY 0)
(def ^:private MAX-HISTORY 10)

(defn- validate-with-exception [validator-fn input]
  (if validator-fn
    (if-not (validator-fn input)
      (throw (new-illegal-state-error "Invalid reference state")))))

(defn- current-val [r]
  (try
    (lock (get-read-lock (.lock r)))
    (if (nil? (.tvals r))
      (throw (new-illegal-state-error (str r " is unbound.")))
      (.val (.tvals r)))
    (finally
      (unlock (get-read-lock (.lock r))))))

(defprotocol ^:private IHistory
  (-get-history-count [this])
  (-get-min-history [this])
  (-set-min-history [this n])
  (-get-max-history [this])
  (-set-max-history [this n]))

(deftype Ref [-state
              -faults
              ^:unsynchronized-mutable -meta
              ^:volatile-mutable -validator
              ^:volatile-mutable -watches
              ^:volatile-mutable -min-history
              ^:volatile-mutable -max-history
              lock]

  IHistory
  (-get-history-count [this])

  (-get-min-history [this] -min-history)

  (-set-min-history [this n]
    (set! -min-history n)
    this)

  (-get-max-history [this] -max-history)

  (-set-max-history [this n]
    (set! -max-history n)
    this)

  IComparable
  (-compare-to [this other])

  IDeref
  (-deref [this]
    (let [t (stm/get-running)]
      (if (nil? t)
        (current-val this)
        (stm/do-get t this))))

  IMeta
  (-meta [this] -meta)

  IReference
  (-reset-meta! [this new-meta]
    (set! -meta new-meta)
    new-meta)

  (-alter-meta! [this f args]
    (let [meta-args (cons -meta args)
          new-meta (apply f meta-args)]
      (reset-meta! this new-meta)))

  IValidatable
  (-get-validator  [this] -validator)

  (-set-validator! [this f]
    (validate-with-exception f (ref-get -state))
    (set! -validator f)
    nil)

  IWatchable
  (-add-watch [this k f]
    (do
      (set! -watches (assoc -watches k f))
      this))

  (-remove-watch [this k]
    (set! -watches (dissoc -watches k))
    this))

(defn -ref-set [ref val])
(defn -alter [ref fn & args])
(defn -commute [ref fn & args])
(defn -ensure [ref])

(defn new-ref [state meta validator watches min-history max-history]
  (Ref.
    state
    (new-atomic-counter 0)
    meta
    validator
    watches
    (or min-history MIN-HISTORY)
    (or max-history MAX-HISTORY)
    (new-read-write-lock)))

