(ns clojure.lang.stm
  (:refer-clojure :only [defn defn- deftype defprotocol apply cond let loop pos? doseq -> when when-not when-let locking])
  (:require [clojure.lang.atomic-ref :refer [new-atomic-ref ref-set!
                                             ref-get ref-compare-and-set!
                                             new-atomic-long]]
            [clojure.lang.atomic-counter :refer [new-atomic-counter get-and-increment-atomic-counter]]
            [clojure.lang.atomic-ref :refer [ref-get ref-compare-and-set!]]
            [clojure.lang.exceptions :refer [platform-try new-retry-exception retry-exception]]
            [clojure.lang.lock :refer [new-read-write-lock get-read-lock get-write-lock lock unlock]]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]
            [clojure.lang.persistent-hash-map :refer [EMPTY-HASH-MAP]]
            [clojure.lang.persistent-sorted-map :refer [EMPTY-SORTED-MAP]]
            [clojure.lang.thread     :refer [local-state get-local-state set-local-state
                                             new-countdown-latch latch-countdown]]
            [clojure.next            :refer :all :exclude [cons]]))

(def ^{:private true} RUNNING 0)
(def ^{:private true} COMMITTING 1)
(def ^{:private true} RETRY 2)
(def ^{:private true} KILLED 3)
(def ^{:private true} COMMITTED 4)

(def ^{:private true} BARGE-WAIT-NANOS (* 10 1000000))
(def ^{:private true} RETRY-LIMIT 1000)

(def ^{:private true} transaction (local-state))

; TO DO
(defn point [tvals])
(defn validate [ref validator entries])
(defn histcount [ref])
(defn new-tval [])
(defn get-faults [ref])
(defn get-max-history [ref])
(defn get-min-history [ref])
(defn get-next [tvals])
(defn get-watches [ref])

(defprotocol IInfo
  (running? [this]))

(deftype Info [status start-point latch]
  IInfo
  (running? [this]
    (let [s (ref-get status)]
      (or (= s RUNNING) (= s COMMITTING)))))

(defn new-info [status start-point]
  (Info. (new-atomic-counter status) start-point (new-countdown-latch 1)))

(defn nano-time [] :system/nano-time)

(defn new-notify [ref oldval newval]
  (-> EMPTY-HASH-MAP
    (assoc :ref ref)
    (assoc :oldval oldval)
    (assoc :newval newval)))

(defprotocol ITransaction
  (-set-read-point [this rp])
  (-set-start-point [this sp])
  (-set-start-time [this st])
  (-set-info [this i])
  (-set-vals [this v])

  (get-info [this])
  (barge [this info])
  (release-if-ensured [this ref])
  (stop [this status])
  (try-write-lock [this ref])
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

  (barge [this info]
    (if (and (> (- (nano-time) start-time) BARGE-WAIT-NANOS)
             (< start-point (.start-point info))
             (ref-compare-and-set! (.status info) RUNNING KILLED))
      (do
        (latch-countdown (.latch info))
        true)
      false))

  (release-if-ensured [this ref]
    (when (contains? ensures ref)
      (set! ensures (disj ensures ref))
      (-> (.lock ref)
          get-read-lock
          unlock)))

  (stop [this status]
    (when info
      (locking info
        (set! (.status info) status)
        (latch-countdown (.latch info)))
      (set! info nil)
      (set! vals (empty vals))
      (set! sets (empty sets))
      (set! commutes (empty commutes))))

  (try-write-lock [this ref])

  (-set-read-point [this rp] (set! read-point rp))
  (-set-start-point [this sp] (set! start-point sp))
  (-set-start-time [this st] (set! start-time st))
  (-set-info [this i] (set! info i))
  (-set-vals [this v] (set! vals v))

  (run [this fn]
    (let [done (atom nil)
          ret (atom nil)
          notifies (atom EMPTY-LIST)
          locked (atom EMPTY-LIST)]
      (loop [i 0]
        (platform-try
          (-set-read-point read-point (get-and-increment-atomic-counter last-point))
          (when (= i 0)
            (-set-start-point start-point read-point)
            (-set-start-time start-time (nano-time)))
          (-set-info info (new-info RUNNING start-point))
          (reset! ret (fn))
          (when (ref-compare-and-set! (.status info) RUNNING COMMITTING)
            (doseq [[ref fns] commutes]
              (when-not (contains? sets ref)
                (let [was-ensured (contains? ensures ref)]
                  (release-if-ensured this ref)
                  (try-write-lock this ref)
                  (swap! locked #(conj % ref))
                  (when (and was-ensured (.tvals ref) (> (point (.tvals ref)) read-point))
                    (throw (new-retry-exception))))
                (let [refinfo '(tinfo ref)]
                  (when (and refinfo (not= refinfo info) (running? refinfo))
                    (when-not (barge this refinfo)
                      (throw (new-retry-exception)))))
                (let [val (when-let [tval (.tvals ref)] (val tval))]
                  (-set-vals vals (assoc vals ref val)))
                (doseq [f fns]
                  (-set-vals vals (assoc vals ref (apply (get vals ref) (:args f)))))))
            (doseq [ref sets]
              (try-write-lock this ref)
              (swap! locked #(conj % ref)))
            (doseq [[ref entry] vals]
              (validate ref (get-validator ref) entry))
            (let [commit-point (get-and-increment-atomic-counter last-point)]
              (doseq [[ref entry] vals]
                (let [oldval (when-let [tval (.tvals ref)] (val tval))
                      hcount (histcount ref)]
                  (cond
                    (not (.tvals ref))
                    (set! (.tvals ref) (new-tval entry commit-point))
                    (or (and (pos? (get-faults ref)) (< hcount (get-max-history ref)))
                        (< hcount (get-min-history ref)))
                    (set! (.tvals ref) (new-tval entry commit-point (.tvals ref)))
                    :else
                    (do
                      (set! (.tvals ref) (get-next (.tvals ref)))
                      (set! (.val (.tvals ref)) entry)
                      (set! (.point (.tvals ref)) commit-point)))
                  (when (seq (get-watches ref))
                    (swap! notifies #(conj % (new-notify ref oldval entry)))))))
            (reset! done true)
            (set! (.status info) COMMITTED))
          (platform-catch retry-exception e)
          (finally
            (doseq [r @locked]
              (-> r
                  get-lock
                  write-lock
                  unlock))
            (swap! locked empty)
            (doseq [ref ensures]
              (-> (.lock ref)
                  get-read-lock
                  unlock))
            (set! ensures (empty ensures))
            (stop this (if @done COMMITTED RETRY))
            (try
              (if @done
                (doseq [n @notifies]
                  (notify-watches (:ref n) (:oldval n) (:newval n)))
                (doseq [a actions]
                  (dispatch-action a)))
              (finally
                (swap! notifies empty)
                (set! actions (empty actions))))))
        (if @done
          @ret
          (if (< i RETRY-LIMIT)
            (recur (inc i))
            (throw "Transaction failed after reaching retry limit")))))))

(defn new-transaction []
  (Transaction.
    (new-atomic-long)
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
