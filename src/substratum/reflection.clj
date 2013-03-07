(ns substratum.reflection
  (:use [substratum.core])
  (:require [clojure.set :as set]))

(defn all-interfaces
  "Returns a sorted list of all interfaces implemented by the class or any
class above it in the hierarchy."
  [^Class class]
  (sort-by #(.getName ^Class %) 
           (filter (fn [^Class x] (.isInterface x))
                   (supers class))))

(defn all-superclasses
  "Returns a sorted list of all super-classes above the class in the hierarchy."
  [^Class class]
  (sort-by #(.getName ^Class %)
           (filter (fn [^Class x] (not (.isInterface x)))
                   (supers class))))
