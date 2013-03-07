(ns substratum.specialized.queue.perf
  (:refer-clojure :exclude [load])
  (:use [substratum.core]
        [clojure.test])
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

(defn dump
  ([colls _ n]
     (-report-on-all colls n #(count (-dump %1))))
  ([colls _ n m]
     (-report-on-all colls n m #(count (-dump %1)))))

(defn head-to-head
  [one two op size n m name1 name2]
  (let [tone (simple-name (type one))
        ttwo (simple-name (type two))
        results (op [one two] size n m)
        om95 (:mean95 ((keyword tone) results))
        tm95 (:mean95 ((keyword ttwo) results))]
    (println (format "%s vs. %s, Size = %s" (or name1 tone) (or name2 ttwo) size))
    (println (format "%s: %.5fms" name1 (/ om95 m)))
    (println (format "%s: %.5fms" name2 (/ tm95 m)))
    (if (< om95 tm95)
      (println (format "Speedup: x%.5f" (/ tm95 om95)))
      (println (format "Slowdown: x%.5f" (/ om95 tm95))))
    (println)))

(deftest realtime-vs-batched
  (head-to-head realtime/empty-queue batched/empty-queue load-and-dump 20 100000 1000 "RealTime" "Batched")
  (head-to-head realtime/empty-queue batched/empty-queue load-and-dump 2 100000 1000 "RealTime" "Batched")
  (head-to-head realtime/empty-queue batched/empty-queue load-and-dump 2000 1000 10 "RealTime" "Batched"))
