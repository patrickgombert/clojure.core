(ns clojure.lang.ref-test
  (:refer-clojure :only [defmacro fn let list list* pos?])
  (:require [clojure.test            :refer :all]
            [clojure.lang.exceptions :refer [illegal-state-error]]
            [clojure.next            :refer :all :exclude []]))

(defmacro illegal-state-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? illegal-state-error msg body)))

(deftest ref-test
  (testing "creates a ref which can be dereferenced"
    (let [new-ref (ref "ref")]
      (is (= "ref" (deref new-ref)))))

  (testing "validates the starting state"
    (illegal-state-error-is-thrown? #"Invalid reference state"
      (ref 0 :validator pos?)))

  (testing "gets the min history"
    (let [new-ref (ref "ref" :min-history 5)]
      (is (= 5 (ref-min-history new-ref)))))

  (testing "sets the min history"
    (let [new-ref (ref "ref")]
      (is (= 0 (ref-min-history new-ref)))
      (is (= 12 (ref-min-history (ref-min-history new-ref 12))))))

  (testing "gets the max history"
    (let [new-ref (ref "ref" :max-history 1)]
      (is (= 1 (ref-max-history new-ref)))))

  (testing "sets the max history"
    (let [new-ref (ref "ref")]
      (is (= 10 (ref-max-history new-ref)))
      (is (= 22 (ref-max-history (ref-max-history new-ref 22))))))

  (testing "get-validator is nil if a validator has not been set"
    (let [new-ref (ref "ref")]
      (is (nil? (get-validator new-ref)))))

  (testing "set-validator! will set the current validator function"
    (let [validator-fn #(not= 5 %)
          new-ref (ref 6)]
      (is (nil? (set-validator! new-ref validator-fn)))
      (is (= validator-fn (get-validator new-ref)))))

  (testing "meta is nil when not defined"
    (let [new-ref (ref "ref")]
      (is (nil? (meta new-ref)))))

  (testing "meta is available if defined"
    (let [mta {:foo :bar}
          new-ref (ref "ref" :meta mta)]
      (is (= mta (meta new-ref)))))

  (testing "reset-meta! will reset the metadata map"
    (let [new-ref (ref "ref" :meta {:first :meta})
          reset-meta-value (reset-meta! new-ref {:second :meta})]
      (is (= {:second :meta} reset-meta-value))
      (is (= {:second :meta} (meta new-ref)))))

  (testing "alter-meta! will reset the metadata map by applying a function"
    (let [new-ref (ref "ref" :meta {:first :meta})
          alter-meta-value (alter-meta! new-ref (fn [_ k v] {k v}) :second :meta)]
      (is (= {:second :meta} alter-meta-value))
      (is (= {:second :meta} (meta new-ref)))))

  #_(testing "add-watch will add a function to be invoked on state changes"
    (let [received-key (atom nil)
          received-atom (atom nil)
          received-old-val (atom nil)
          received-new-val (atom nil)
          watch-fn (fn [k a old nw]
                       (reset! received-key k)
                       (reset! received-atom a)
                       (reset! received-old-val old)
                       (reset! received-new-val nw))
          new-ref (add-watch (ref "old") :add-watch watch-fn)]
      (ref-set new-ref "new")
      (is (and (= :add-watch (deref received-key))
               (= atm (deref received-atom))
               (= "old" (deref received-old-val))
               (= "new" (deref received-new-val))))))

  #_(testing "add-watch will overwrite watch-keys"
    (let [received-key (atom nil)
          received-atom (atom nil)
          received-old-val (atom nil)
          received-new-val (atom nil)
          old-watch-fn (fn [k a old nw]
                       (reset! received-key "fail")
                       (reset! received-atom "fail")
                       (reset! received-old-val "fail")
                       (reset! received-new-val "fail"))
          watch-fn (fn [k a old nw]
                       (reset! received-key k)
                       (reset! received-atom a)
                       (reset! received-old-val old)
                       (reset! received-new-val nw))
          tmp-ref (add-watch (ref "old") :add-watch old-watch-fn)
          new-ref (add-watch tmp-ref :add-watch watch-fn)]
      (ref-set new-ref "new")
      (is (and (= :add-watch (deref received-key))
               (= atm (deref received-atom))
               (= "old" (deref received-old-val))
               (= "new" (deref received-new-val))))))

  #_(testing "remove-watch will remove a watch key"
    (let [received-key (atom nil)
          received-atom (atom nil)
          received-old-val (atom nil)
          received-new-val (atom nil)
          watch-fn (fn [k a old nw]
                       (reset! received-key k)
                       (reset! received-atom a)
                       (reset! received-old-val old)
                       (reset! received-new-val nw))
          tmp-ref (add-watch (atom "old") :add-watch watch-fn)
          new-ref (remove-watch tmp-atm :add-watch)]
      (ref-set new-ref "new")
      (is (and (nil? (deref received-key))
               (nil? (deref received-atom))
               (nil? (deref received-old-val))
               (nil? (deref received-new-val)))))))
