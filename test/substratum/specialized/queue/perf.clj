(ns substratum.specialized.queue.perf
  (:refer-clojure :exclude [load])
  (:use [substratum.core])
  (:require [substratum.benchmark :as bm]
            [substratum.specialized.queue.batched :as batched]
            [substratum.specialized.queue.realtime :as realtime])
  (:import [clojure.lang IPersistentStack IPersistentCollection]))

(defconst -val
  42)

(defn -load [^IPersistentCollection coll ^long size]
  (loop [i 0
         ^IPersistentCollection coll coll]
    (if (>= i size)
      coll
      (recur (inc i) (.cons coll -val)))))

(defn -dump [^IPersistentStack coll]
  (loop [^IPersistentStack coll coll]
    (if (peek coll)
      (recur (pop coll))
      coll)))

(defn -into [coll src]
  (into coll src))

(defn- simple-name
  [^Class class]
  (.getSimpleName class))

(defmacro -report-on-all
  ([xs n f]
     `(map->map #(list (keyword (simple-name (type %1)))
                       (bm/time-stats ~n (fn [] (~f %1))))
                ~xs))
  ([xs n m f]
     `(map->map #(list (keyword (simple-name (type %1)))
                       (bm/time-stats ~n ~m (fn [] (~f %1))))
                ~xs)))

(defn load
  ([colls size n]
     (-report-on-all colls n #(count (-load %1 size))))
  ([colls size n m]
     (-report-on-all colls n m #(count (-load %1 size)))))

(defn load-and-dump
  ([colls size n]
     (-report-on-all colls n #(count (-dump (-load %1 size)))))
  ([colls size n m]
     (-report-on-all colls n m #(count (-dump (-load %1 size))))))

(defn into-and-dump
  ([colls size n]
     (let [v (vec (repeat size -val))]
       (-report-on-all colls n #(count (-dump (-into %1 v))))))
  ([colls size n m]
     (let [v (vec (repeat size -val))]
      (-report-on-all colls n m #(count (-dump (-into %1 v)))))))

