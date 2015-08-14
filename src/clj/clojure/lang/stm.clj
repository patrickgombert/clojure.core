(ns clojure.lang.stm
  (:refer-clojure :only [defn defn- deftype let binding])
  (:require [clojure.lang.atomic-ref :refer [new-atomic-ref ref-set!
                                             ref-get ref-compare-and-set!
                                             new-atomic-long]]
            [clojure.lang.atomic-counter :refer [new-atomic-counter get-and-increment-atomic-counter]]
            [clojure.lang.atomic-ref :refer [ref-get ref-compare-and-set!]]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]
            [clojure.lang.persistent-hash-map :refer [EMPTY-HASH-MAP]]
            [clojure.lang.persistent-sorted-map :refer [EMPTY-SORTED-MAP]]
            [clojure.lang.thread     :refer [local-state get-local-state set-local-state
                                             new-countdown-latch]]
            [clojure.next            :refer :all :exclude [cons]]))

(def ^{:private true} RUNNING 0)
(def ^{:private true} COMMITTING 1)
(def ^{:private true} RETRY 2)
(def ^{:private true} KILLED 3)
(def ^{:private true} COMMITTED 4)

(def ^{:private true} transaction (local-state))

(defprotocol IInfo
  (running? [this]))

(deftype Info [status start-point latch]
  IInfo
  (running? [this]
    (let [s (ref-get status)]
      (or (= s RUNNING) (= s COMMITTING)))))

(defn new-info [status start-point]
  (Info. (new-atomic-counter status) start-point (new-countdown-latch 1)))

(defprotocol ITransaction
  (get-info [this])
  (run [this fn]))

(deftype Transaction
  [last-point
   ^:volatile-mutable read-point
   ^:volatile-mutable start-point
   ^:volatile-mutable start-time
   ^:volatile-mutable actions
   ^:volatile-mutable vals
   ^:volatile-mutable sets
   ^:volatile-mutable commutes
   ^:volatile-mutable ensures
   ^:volatile-mutable info]

  ITransaction
  (get-info [this])

  (run [this fn]
    (loop [i 0 done false ret nil]
      (set! read-point (get-and-increment-atomic-counter last-point))
      (if (= i 0)
        (set! start-point read-point)
        (set! start-time (nano-time)))
      (set! info (new-info RUNNING start-point))
      (let [ret (fn)]
        (if (ref-compare-and-set! (.status info) RUNNING COMMITTING)
          (loop [[ref fns :as c] (first commutes) cs (rest commutes)]
            (when c
              (when-not (contains? sets ref)
                (let [was-ensured (contains? ensures ref)]
                  (release-if-ensured ref)
                  (try-write-lock ref)
                  (add-lock locked ref)
                  (when (and was-ensured (tvals ref) (> (point (tvals ref)) read-point))
                    (throw retry-ex))
                  (let [refinfo (tinfo ref)]
                    (when (and refinfo (not= refinfo info) (running? refinfo))
                      (when-not (barge! refinfo)
                        (throw retry-ex))))
                  (let [val (when-let [tval (tvals ref)] (val tval))]
                    (set! vals (assoc vals ref val)))
                  (loop [ffn (first fns) rfns (rest fns)]
                    (when ffn
                      (set! vals (assoc vals ref (wtf)))
                      (recur (first rfns) (rest rfns))))))
              (recur (first cs) (rest cs))))
          (loop [ref (first sets) rs (rest sets)]
            (when ref
              (try-write-lock ref)
              (add-lock locked ref)
              (recur (first rs) (rest rs))))


(defn new-transaction []
  (Transaction.
    (atomic-long)
    nil
    nil
    nil
    EMPTY-LIST
    EMPTY-HASH-MAP
    (hash-set)
    EMPTY-SORTED-MAP
    (hash-set)
    nil))

(defn run-in-transaction [fn]
  (let [t (get-local-state transaction)]
    (if t
      (do
        (set-local-state transaction (new-transaction))
        (let [ret (run t fn)]
          (set-local-state transaction nil)
          ret))
      (if (get-info t)
        (fn)
        (run t fn)))))
