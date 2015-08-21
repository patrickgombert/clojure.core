(ns clojure.lang.stm
  (:refer-clojure :only [defn defn- deftype let binding doseq])
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
    (let [done (atom nil)
          ret (atom nil)]
      (loop [i 0]
        (try
          (set! read-point (get-and-increment-atomic-counter last-point))
          (when (= i 0)
            (set! start-point read-point)
            (set! start-time (nano-time)))
          (set! info (new-info RUNNING start-point))
          (reset! ret (fn))
          (when (ref-compare-and-set! (.status info) RUNNING COMMITTING)
            (doseq [[ref fns] commutes]
              (when-not (contains? sets ref)
                (let [was-ensured (contains? ensures ref)]
                  (release-if-ensured ref)
                  (try-write-lock ref)
                  (add-lock locked ref)
                  (when (and was-ensured (tvals ref) (> (point (tvals ref)) read-point))
                    (throw retry-ex)))
                (let [refinfo (tinfo ref)]
                  (when (and refinfo (not= refinfo info) (running? refinfo))
                    (when-not (barge! refinfo)
                      (throw retry-ex))))
                (let [val (when-let [tval (tvals ref)] (val tval))]
                  (set! vals (assoc vals ref val)))
                (doseq [f fns]
                  (set! vals (assoc vals ref (wtf))))))
            (doseq [ref sets]
              (try-write-lock ref)
              (add-lock locked ref))
            (doseq [[ref entry] vals]
              (validate ref (get-validator ref) entry))
            (let [commit-point (get-and-increment-atomic-counter last-point)]
              (doseq [[ref entry] vals]
                (let [oldval (when-let [tval (tvals ref)] (val tval))
                      hcount (histcount ref)]
                  (cond
                    (not (tvals ref))
                    (set! (tvals ref) (new-tval entry commit-point))
                    (or (and (pos? (get-faults ref)) (< hcount (get-max-history ref)))
                        (< hcount (get-min-history ref)))
                    (set! (tvals ref) (new-tval entry commit-point (tvals ref)))
                    :else
                    (do
                      (set! (tvals ref) (get-next (tvals ref)))
                      (set! (val (tvals ref)) entry)
                      (set! (point (tvals ref)) commit-point)))
                  (when (seq (get-watches ref))
                    (add-notify (new-notify ref oldval entry))))))
            (reset! done true)
            (set! (.status info) COMMITTED))
          (catch retry-ex)
          (finally
            (doseq [i (range (count locked))]
              (-> (get locked i)
                  get-lock
                  write-lock
                  unlock))
            (clear locked)
            (doseq [ref ensures]
              (-> (get-lock ref)
                  read-lock
                  unlock))
            (clear ensures)
            (stop (if @done COMMITTED RETRY))
            (try
              (if @done
                (doseq [n notifies]
                  (notify-watches (get-ref n) (get-oldval n) (get-newval n)))
                (doseq [a actions]
                  (dispatch-action a)))
              (finally
                (clear notifies)
                (clear actions)))))
        (if @done
          @ret
          (if (< i RETRY_LIMIT)
            (recur (inc i))
            (throw "Transaction failed after reaching retry limit")))))))

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
