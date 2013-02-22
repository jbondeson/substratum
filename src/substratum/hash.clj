(ns substratum.hash
  "Hashing helpers")

(defmacro hash!
  "Takes in an object to hash, a function to use to compute the hash
and the mutable variable to store the hash in. If the hash has already
been computed, it simply returns the values, otherwise it calculates
and stores the new value."
  [x hash-fn hash-var]
  `(let [h# ~hash-var]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~x)]
         (set! ~hash-var h#)
         h#))))

(defn hash-coll
  "Calculate the hash of a collection using `hash-combine`"
  [coll]
  (reduce #(hash-combine %1 (hash %2)) (hash (first coll)) (next coll)))

(defn hash-list
  "Calculate the hash of list ala `clojure.lang.PersistentList`"
  [coll]
  (reduce #(+ (* 31 %1) (hash %2)) 1 coll))

(defn hash-imap
  "Calculate the hash of a map ala `clojure.lang.APersistentMap`"
  [m]
  (loop [h 0 s (seq m)]
    (if s
      (let [e (first s)]
        (recur (mod (+ h (bit-xor (hash (key e)) (hash (val e))))
                    4503599627370496)
               (next s)))
      h)))

(defn hash-iset [s]
  "Calculate the hash of a set ala `clojure.lang.APersistentSet`"
  (loop [h 0 s (seq s)]
    (if s
      (let [e (first s)]
        (recur (mod (+ h (hash e)) 4503599627370496)
               (next s)))
      h)))
