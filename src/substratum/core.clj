(ns substratum.core
  "A set of ubiquitous functions")

(set! *warn-on-reflection* true)

;; # Variants of `def` forms.

(defmacro def-
  "Same as `def`, yielding a non-public var"
  [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defmacro-
  "Same as 'defmacro`, yielding a non-public var"
  [name & decls]
  (list* `defmacro (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defonce-
  "Same as 'defonce`, yielding a non-public var"
  [name & decls]
  (list* `defonce (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defconst
  [name & decls]
  (list* `def (with-meta name (assoc (meta name) :const true)) decls))

(defmacro defconst-
  [name & decls]
  (list* `def (with-meta name
                (assoc (meta name)
                  :const true :private true))
         decls))

;; # Sequence Functions

(defn count-if
  ([pred xs]
     (count (filter pred xs)))
  ([pred xs f]
     (count (filter (comp pred f) xs))))

(defn unchunk
  "Takes a seqable and returns a lazy sequence that
   is maximally lazy and doesn't realize elements due to either
   chunking or apply.

   Taken from http://stackoverflow.com/a/3409568"
  [s]
  (when (seq s)
    (cons (first s)
          (lazy-seq (unchunk (rest s))))))

(defn map->map
  "Returns the result of applying concat to the result of applying map
to f and colls and then building a map. Thus function f should return
a collection."
  [f & colls]
  (apply hash-map
         (apply concat
                (apply map f colls))))

(set! *warn-on-reflection* false)

