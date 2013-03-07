(ns substratum.benchmark)

(defmacro timed
  "Return the elasped time in milliseconds for an expression to evaluate.
NOTE: This does not evaluate results, so any lazy effects will need to be
forced in the expression to get an accurate timing."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         stop# (. System (nanoTime))
         diff# (- stop# start#)]
     (/ (double diff#) 1000000.0)))

(defn basic-stats
  [coll]
  (let [[y & ys] coll]
    (loop [[x & xs] ys
           sum y
           sumsq (* sum sum)
           hi y
           lo y
           cnt 1]
      (if x
        (let [sum (+ sum x)
              sumsq (+ sumsq (* x x))
              hi (max hi x)
              lo (min lo x)
              cnt (inc cnt)]
          (recur xs sum sumsq hi lo cnt))
        (let [mean (/ sum cnt)
              variance (Math/abs (double (- (/ sumsq cnt) mean)))]
          {:sum sum :sum-squares sumsq :max hi :min lo :count cnt
           :mean (/ sum cnt) :variance variance :std-dev (Math/sqrt (double variance))})))))

(defn adv-stats
  [coll]
  (let [coll (vec coll)
        coll (sort coll)
        basic (basic-stats coll)
        cnt (count coll)
        stats95 (basic-stats (take (int (* 0.95 cnt)) coll))
        ]
    (assoc basic
      :median (nth coll (int (/ (dec cnt) 2)))
      :mean95 (:mean stats95)
      :max95 (:max stats95))))

(defn -time-stats
  [n f]
  (let [cold (doall (repeatedly n f))
        warm (doall (repeatedly n f))]
    (adv-stats warm)))

(defmacro time-stats2
  "Compute execution statistics for executing a function"
  ([n expr]
     `(let [f# #(timed ~expr)]
        (-time-stats ~n f#)))
  ([n m expr]
     `(let [f# #(timed (doall (repeatedly ~m (fn [] ~expr))))]
        (-time-stats ~(int (/ n m)) f#))))

(defn time-stats
  ([n f]
     (time-stats n 1 f))
  ([n m f]
     (let [f (if (< m 2)
               #(timed (f))
               #(timed (doall (repeatedly m f))))
           n (if (< m 2) n (int (/ n m)))
           cold (doall (repeatedly n f))
           warm (doall (repeatedly n f))]
       (adv-stats warm))))
