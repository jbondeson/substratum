(ns substratum.specialized.queue.realtime
  "Persistent Realtime Queue Implementation."
  (:use [substratum.core])
  (:require [substratum.hash :as hash]
            [substratum.seq :as seq])
  (:import [clojure.lang IObj IHashEq ISeq Sequential Seqable IPersistentList
            IPersistentStack Counted IPersistentCollection IEditableCollection
            Cons PersistentList]))

(deftype RealTimeQueue [a b c d e f])

(declare exec exec-seq)

(deftype EmptyRealTimeQueue [meta]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] 0)
  (toString [_] (str "(), (), ()" ))
  IObj
  (meta [_] meta)
  (withMeta [_ meta] (EmptyRealTimeQueue. meta))
  IHashEq
  (hasheq [this] 0)
  Sequential
  Seqable
  (seq [_] nil)
  IPersistentList
  IPersistentStack
  (peek [_] nil)
  (pop [this] this)
  Counted
  IPersistentCollection
  (count [_] 0)
  (empty [this] this)
  (equiv [this x] (.equals this x))
  (cons [_ x] (exec meta 1 nil (list x) nil)))

(deftype RealTimeQueueSeq [meta front rear acc ^:unsynchronized-mutable __hash]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] (hash/hash! this hash/hash-list __hash))
  IObj
  (meta [_] meta)
  (withMeta [_ meta] (RealTimeQueueSeq. meta front acc rear __hash))
  IHashEq
  (hasheq [this] (.hashCode this))
  ISeq
  (first [_] (first front))
  (next [_] (let [front (next front)]
              (if (or front rear)
                (exec-seq meta front rear acc))))
  (more [this] (or (.next this) PersistentList/EMPTY))
  (cons [this x] (Cons. meta x this))
  Sequential
  Seqable
  (seq [this] this)
  IPersistentCollection
  (count [_] (+ (count front) (count rear)))
  (empty [_] PersistentList/EMPTY)
  (equiv [this x] (.equals this x)))

(deftype RealTimeQueue [meta cnt front rear acc ^:unsynchronized-mutable __hash]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] (hash/hash! this hash/hash-list __hash))
  ;;(toString [_] (str (seq/seq->str front) ", " (seq/seq->str rear) ", " (seq/seq->str acc)))
  (toString [_] (str front) ", " (str rear) ", " (str acc))
  IObj
  (meta [_] meta)
  (withMeta [_ meta] (RealTimeQueue. meta cnt front rear acc __hash))
  IHashEq
  (hasheq [this] (.hashCode this))
  Sequential
  Seqable
  (seq [_] (RealTimeQueueSeq. meta front rear acc nil))
  IPersistentList
  IPersistentStack
  (peek [_] (first front))
  (pop [this] (let [front (next front)]
                (if (or front rear)
                  (exec meta (dec cnt) front rear acc)
                  (EmptyRealTimeQueue. meta))))
  Counted
  IPersistentCollection
  (count [_] cnt)
  (empty [_] (EmptyRealTimeQueue. meta))
  (equiv [this x] (.equals this x))
  (cons [_ x] (exec meta (inc cnt) front (cons x rear) acc)))

(defn- rotate [xs ys acc]
    (let [y (first ys)]
      (if (nil? xs)
        (lazy-seq (cons y acc))
        (let [x (first xs)
              xs (next xs)
              ys (next ys)]
          (lazy-seq (cons x (rotate xs ys (lazy-seq (cons y acc)))))))))

(defn- exec
  [meta cnt front rear acc]
  (if (nil? acc)
    (let [rot (rotate front rear nil)]
      (RealTimeQueue. meta cnt rot nil rot nil))
    (RealTimeQueue. meta cnt front rear (next acc) nil)))

(defn- exec-seq
  [meta front rear acc]
  (let [a (first acc)
        acc (next acc)]
    (if (and (nil? a) (nil? acc))
      (let [rot (rotate front rear nil)]
        (RealTimeQueueSeq. meta rot nil rot nil))
      (RealTimeQueueSeq. meta front rear acc nil))))

(def empty-queue
  (EmptyRealTimeQueue. nil))

(defn queue
  ([] empty-queue)
  ([& args]
     (apply into empty-queue args)))
