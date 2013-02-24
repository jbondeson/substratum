(ns substratum.specialized.seq.weaklazy
  (:require [substratum.hash :as hash]
            [substratum.seq :as seq])
  (:import [clojure.lang IFn ISeq Seqable Sequential
            IPersistentCollection IPending IHashEq IMeta IObj
            PersistentList Util]
           [java.util.concurrent.atomic AtomicReference])
  )

(defprotocol IWeakLazySeq
  (sval [this]))

;; When initialized with a suspended value in f, val holds a reference
;; to a marker object and seq is nil to ensure the correct ordering.
(deftype WeakLazySeq [meta
                      ^IFn ^:volatile-mutable f
                      ^AtomicReference val
                      ^Boolean ^:volatile-mutable forced?
                      ^ISeq ^:volatile-mutable s
                      ^Object marker]
  Object
  (equals [this x] (seq/equiv-sequential this x))
  (hashCode [this] (if-let [x (seq this)]
                     (Util/hash x)
                     1))
  (toString [_] (if forced? "Forced" (if f "Unrealized" "Unforced")))
  IMeta
  (meta [_] meta)
  IObj
  (withMeta [this meta] (WeakLazySeq. meta nil nil true (seq this) nil))
  IHashEq
  (hasheq [this] (if-let [x (seq this)]
                   (Util/hasheq x)
                   1))
  IWeakLazySeq
  (sval [_] (if forced?
              s
              (do (if-let [cf f]
                    (let [nval (cf)]
                      ;; We don't care about the return from
                      ;; `compareAndSet` as spuriously setting the
                      ;; value of `f` to nil twice is not an issue.
                      ;; Once `val` has been forced to a value other
                      ;; than `marker` we are good. Additionally we
                      ;; want to set `f` to nil as soon as possible so
                      ;; we minimize the number of invokations of `f`.
                      (.compareAndSet val marker nval)
                      (set! f nil)))
                  ;; Theoretically a second thread could have
                  ;; called `seq` and forced `val` into `s`, so we
                  ;; need to re-check the value stored in `val`.
                  (or (.get val) s))))
  Sequential
  Seqable
  ;; WARNING: there is one scenario where this object could have two
  ;; different valid `s` values, if a thread enters the when, sets
  ;; `s`, but gets suspended before setting `forced?` while a second
  ;; thread enters the `seq` function. There would be a window after
  ;; testing that `cs` and `s`  were identical but before and setting
  ;; `s` to the new value where a third thread could call `sval` and
  ;; retrieve the first thread's value for `s`. At that point you
  ;; would have two "live" seq values. Natrually this would only
  ;; happen with heavy use of persistence with chained WeakLazySeqs.
  (seq [this] (if forced?
                s
                (let [cs s
                      ns (loop [ns (sval this)]
                           (if (instance? WeakLazySeq ns)
                             (recur (sval ns))
                             ns))
                      ns (seq ns)]
                  ;; If the value in `s` changed we know someone beat
                  ;; us to the punch, so we can use there value.
                  
                  (when (identical? cs s)
                    (set! s ns))
                  (set! forced? true)
                  (.set val nil)
                  ;; Due to the `s` being volatile a memory-fence will
                  ;; allow a thread that failed to update `s` to get
                  ;; the new value.
                  s)))
  ISeq
  (first [this] (first (seq this)))
  (next [this] (next (seq this)))
  (more [this] (rest (seq this)))
  IPersistentCollection
  (count [this] (count (seq this)))
  (empty [_] PersistentList/EMPTY)
  (equiv [this x] (.equals this x))
  (cons [this x] (cons x (seq this)))
  IPending
  (isRealized [_] (nil? f))
  )

(defmacro weak-lazy-seq
  "Like `lazy-seq`, but does not provide as strong of guarantees around
how many times the function will be executed, or that and identical seq
will be generated on every function that yields a `seq`."
  [& body]
  `(let [marker# (Object.)]
     (new substratum.specialized.seq.weaklazy.WeakLazySeq nil
            ~(list* '^:once fn* [] body)
            (java.util.concurrent.atomic.AtomicReference. marker#)
            false nil marker#)))





