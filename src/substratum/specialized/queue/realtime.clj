(ns substratum.specialized.queue.realtime
  "Persistent Realtime Queue Implementation."
  (:use [substratum.core])
  (:require [substratum.hash :as hash]
            [substratum.seq :as seq])
  (:import [clojure.lang IObj IHashEq ISeq Sequential Seqable IPersistentList
            IPersistentStack Counted IPersistentCollection IEditableCollection
            Cons PersistentList]))

(set! *warn-on-reflection* true)

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
  (toString [_] (str (seq/seq->str front) ", " (seq/seq->str rear) ", " (seq/seq->str acc)))
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
  (cons [_ x] (exec meta (inc cnt) front (conj rear x) acc)))

(defn- rotate [[x & xs] [y & ys] acc]
  (if (and (nil? x) (nil? xs))
    (lazy-seq (cons y acc))
    (lazy-seq (cons x (rotate xs ys (lazy-seq (cons y acc)))))))

(defn- exec
  [meta cnt front rear [a & s]]
  (if (and (nil? a) (nil? s))
    (let [rot (rotate front rear nil)]
      (RealTimeQueue. meta cnt rot nil rot nil))
    (RealTimeQueue. meta cnt front rear s nil)))

(defn- exec-seq
  [meta front rear [a & s]]
  (if (and (nil? a) (nil? s))
    (let [rot (rotate front rear nil)]
      (RealTimeQueueSeq. meta rot nil rot nil))
    (RealTimeQueueSeq. meta front rear s nil)))

(def empty-queue
  (EmptyRealTimeQueue. nil))

(defn queue
  ([] empty-queue)
  ([& args]
     (apply into empty-queue args)))

(set! *warn-on-reflection* false)
