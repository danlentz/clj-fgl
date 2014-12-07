(ns fgl.core
  (:use [clj-tuple]
        [print.foo]))



(defonce -count- (atom 0))


(defn node
  ([] (swap! -count- inc))
  ([n] (assert (integer? n)) n))

(defn n-tuple [size]
  (apply tuple (repeatedly size node)))


(defn edge [n b]
  [n b])

