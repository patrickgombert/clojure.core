(ns clojure.lang.array
  (:refer-clojure :only [cons defmacro defn dotimes let])
  (:require [clojure.next :refer :all :exclude [cons]])
  (:import [java.lang.reflect Array]))

(defn make-array
  ([^Class c size]
    (Array/newInstance c size))
  ([^Class c d ds]
    (let [dims (cons d ds)
          ^"[I" dimarray (make-array Integer/TYPE (count dims))]
      (dotimes [i (Array/getLength dimarray)]
        (Array/setInt dimarray i (int (clojure.core/nth dims i))))
      (Array/newInstance c dimarray))))

(defmacro array-set! [arr idx v]
  `(Array/set ~arr ~idx ~v))

(defmacro array-get [arr idx]
  `(Array/get ~arr ~idx))

(defmacro array-copy [src src-pos dest dest-pos length]
  `(System/arraycopy ~src ~src-pos ~dest ~dest-pos ~length))

(defmacro array-length [arr]
  `(Array/getLength ~arr))

(defmacro array-clone [arr]
  `(let [arr# ~arr
         size# (array-length arr#)
         new-arr# (make-array Object size#)]
     (array-copy arr# 0 new-arr# 0 size#)
     new-arr#))

(defmacro array-set-byte! [arr idx v]
  `(Array/setByte ~arr ~idx ~v))

(defmacro array-set-short! [arr idx v]
  `(Array/setShort ~arr ~idx ~v))

(defmacro array-set-int! [arr idx v]
  `(Array/setInt ~arr ~idx ~v))

(defmacro array-set-long! [arr idx v]
  `(Array/setLong ~arr ~idx ~v))

(defmacro array-set-float! [arr idx v]
  `(Array/setFloat ~arr ~idx ~v))

(defmacro array-set-double! [arr idx v]
  `(Array/setDouble ~arr ~idx ~v))

