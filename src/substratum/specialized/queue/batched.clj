(ns substratum.specialized.queue.batched
  "Batched Queue Implementation."
  (:require [substratum.hash :as hash]
            [substratum.seq :as seq])
  (:import [clojure.lang IObj IHashEq ISeq Sequential Seqable IPersistentList
            IPersistentStack Counted IPersistentCollection IEditableCollection
            Cons PersistentList ITransientCollection PersistentVector
            ITransientVector]))

(defn- revcons
  [^ISeq coll]
  (loop [^ISeq coll coll
         ^ISeq acc PersistentList/EMPTY]
    (if coll
      (recur (.next coll) (.cons acc (.first coll)))
      acc)))

(deftype BatchedQueue [a b c d e])
(deftype TransientBatchedQueue [a])

(deftype EmptyBatchedQueue [meta]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] 0)
  (toString [_] (str "(), ()" ))
  IObj
  (meta [_] meta)
  (withMeta [_ meta] (EmptyBatchedQueue. meta))
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
  (cons [_ x] (BatchedQueue. meta 1 (Cons. x nil) nil nil))
  IEditableCollection
  (asTransient [this] (TransientBatchedQueue. (transient PersistentVector/EMPTY))))

(deftype BatchedQueueSeq [meta front rear ^:unsynchronized-mutable __hash]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] (hash/hash! this hash/hash-list __hash))
  IObj
  (meta [_] meta)
  (withMeta [_ meta] (BatchedQueueSeq. meta front rear __hash))
  IHashEq
  (hasheq [this] (.hashCode this))
  ISeq
  (first [_] (first front))
  (next [_] (if-let [front (next front)]
              (BatchedQueueSeq. meta front rear nil)
              (when-not (empty? rear)
                (BatchedQueueSeq. meta (revcons rear) nil nil))))
  (more [this] (or (.next this) PersistentList/EMPTY))
  (cons [this x] (Cons. meta x this))
  Sequential
  Seqable
  (seq [this] this)
  IPersistentCollection
  (count [_] (+ (count front) (count rear)))
  (empty [_] PersistentList/EMPTY)
  (equiv [this x] (.equals this x)))

(deftype BatchedQueue [meta cnt ^ISeq front ^ISeq rear ^:unsynchronized-mutable __hash]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] (hash/hash! this hash/hash-list __hash))
  (toString [_] (str (seq/seq->str front) ", " (seq/seq->str rear)))
  IObj
  (meta [_] meta)
  (withMeta [_ meta] (BatchedQueue. meta cnt front rear __hash))
  IHashEq
  (hasheq [this] (.hashCode this))
  Sequential
  Seqable
  (seq [_] (BatchedQueueSeq. meta front rear nil))
  IPersistentList
  IPersistentStack
  (peek [_] (.first front))
  (pop [this] (if-let [^ISeq front (.next front)]
                (BatchedQueue. meta (dec cnt) front rear nil)
                (if (nil? rear)
                  (EmptyBatchedQueue. meta)
                  (BatchedQueue. meta (dec cnt) (revcons rear) nil nil))))
  Counted
  IPersistentCollection
  (count [_] cnt)
  (empty [_] (EmptyBatchedQueue. meta))
  (equiv [this x] (.equals this x))
  (cons [_ x] (BatchedQueue. meta (inc cnt) front (cons x rear) nil))
  IEditableCollection
  (asTransient [this] (let [queue (transient PersistentVector/EMPTY)]
                        (doseq [x (seq this)] (conj! queue x))
                        (TransientBatchedQueue. queue))))

(deftype TransientBatchedQueue [^ITransientVector queue]
  Object
  (toString [_] (str "size: " (count queue)))
  Counted
  (count [_] (count queue))
  ITransientCollection
  (conj [this x] (do
                   (conj! queue x)
                   this))
  (persistent [_] (let [cnt (count queue)]
                    (if (> cnt 0)
                      (let [front (persistent! queue)]
                        (BatchedQueue. nil cnt (lazy-seq front) nil nil))
                      (EmptyBatchedQueue. nil)))))

(def empty-queue
  "An empty queue object."
  (EmptyBatchedQueue. nil))

(defn queue
  "Returns a new queue with the supplied values."
  ([] empty-queue)
  ([& args] (apply into empty-queue args)))
