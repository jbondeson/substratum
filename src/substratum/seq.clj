(ns substratum.seq)

(defn equiv-sequential
  "Assumes x is sequential. Returns true if x equals y, otherwise
returns false."
  [x y]
  (boolean
   (when (sequential? y)
     (loop [xs (seq x) ys (seq y)]
       (cond (nil? xs) (nil? ys)
             (nil? ys) false
             (= (first xs) (first ys)) (recur (next xs) (next ys))
             :else false)))))

(defn seq->str
  "Turns a sequence into a presentable string."
  [coll]
  (let [coll (seq (take 5 coll))
        scoll (if coll (str coll) "()")]
    (if (= (count coll) 5)
      (.replace scoll ")" " ..)")
      scoll)))
