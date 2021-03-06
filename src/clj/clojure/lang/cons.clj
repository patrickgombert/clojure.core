(ns clojure.lang.cons
  (:refer-clojure :only [declare defn list])
  (:require [clojure.next           :refer :all]
            [clojure.lang
              [aseq      :refer [defseq]]
              [deftype]
              [equivalence]
              [protocols :refer [ICounted IMeta IObj
                                 ISeq ISeqable ISequential]]]))

(declare make-cons)

(defseq Cons [-meta -first -more]
  ICounted
  (-count [this]
    (inc (count -more)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this mta]
    (make-cons mta -first -more))

  ISeq
  (-first [this] -first)

  (-next [this] (seq -more))

  (-more [this]
    (if (nil? -more)
      (list)
      -more)))

(defn make-cons
  ([elem s]
    (Cons. {} elem s))
  ([mta elem s]
    (Cons. mta elem s)))
