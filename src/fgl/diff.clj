(ns fgl.diff
  "map differencing and patching"
  (:require [fgl.util :as utx])
  (:use     [print.foo]))


;;; TODO: test suite

(defn merge*
  "Recursively merge 0 or more hierarchically nested maps, returning a new
  map that contains a composite all data."
  [& maps]
  (if (every? map? maps)
    (apply merge-with merge* maps)
    (last maps)))


;; NOTE: cassius diff algo

(defn diff-inserts
  ([m1 m2]
     (diff-inserts m1 m2 [] (atom {})))
  ([m1 m2 pv summary]
     (let [ks (keys m1)]
       (doseq [k ks]
         (let [v1 (get m1 k)
               v2 (get m2 k)]
           (cond
             (and (map? v1) (map? v2)) (diff-inserts v1 v2
                                         (conj pv k) summary)
             (nil? v2)                 (swap! summary conj
                                         [(conj pv k) v1]))))
       @summary)))


(defn diff-changes
  ([m1 m2]
     (diff-changes m1 m2 [] (atom {})))
  ([m1 m2 pv summary]
     (let [ks (keys m1)]
       (doseq [k ks]
         (let [v1 (get m1 k)
               v2 (get m2 k)]
           (cond
             (and (map? v1) (map? v2))  (diff-changes v1 v2
                                          (conj pv k) summary)
             (nil? v2)                  nil
             (not= v1 v2)               (swap! summary conj
                                          [(conj pv k) [v1 v2]]))))
       @summary)))


(defn- maybe-assoc [m k v]
  (when (seq v)
    (assoc m k v)))

(defn- assoc-full [m1 m2]
  (into m1
    (filter (comp #(and (coll? %) (seq %)) second)
      (seq m2))))


(defn diff 
  "return a map of insertions (:+) deletions (:-) and changes (:*)
  required to change (possibly nested) maps 'old' to 'new'"
  [old new]
  (let [v+ (diff-inserts new old)
        v- (diff-inserts old new)
        v* (diff-changes old new)
        m  (assoc-full {} {:+ v+ :- v- :* v*})]
    (if-not (empty? m) m)))



(defn ins [m [ks v]]
  (assert (map? m))
  (apply assoc-in m ks [v]))

(defn ins-unchecked [m [ks v]]
  (apply assoc-in m ks [v]))


(defn del [m [ks v]]
  (assert (map? m))
  (assert (= (get-in m ks) v))
  (if (= 1 (count ks))
    (dissoc m (first ks))
    (update-in m (subvec ks 0 (dec (count ks)))
      dissoc (last ks))))

(defn del-unchecked [m [ks v]]
  (if (= 1 (count ks))
    (dissoc m (first ks))
    (update-in m (subvec ks 0 (dec (count ks)))
      dissoc (last ks))))


(defn chg [m [ks [old new]]]
  (assert (map? m))
  (assert (= (get-in m ks) old))
  (if (= 1 (count ks))
    (assoc m (first ks) new)
    (update-in m (subvec ks 0 (dec (count ks)))
      assoc (last ks) new)))

(defn chg-unchecked [m [ks [old new]]]
  (if (= 1 (count ks))
    (assoc m (first ks) new)
    (update-in m (subvec ks 0 (dec (count ks)))
      assoc (last ks) new)))


(defn patch 
  "Apply a sequence of transformations to map 'm' specified by patch 'p' and
  return a new map with the result.  Patch changes will be checked for
  contextual validity and patch application will fail if differences are
  not congruent with map 'm'."
  [m p]
  (assert (map? m))
  (assert (or (nil? p) (map? p)))
  (-> m
    ((partial reduce del) (seq (:- p)))
    ((partial reduce ins) (seq (:+ p)))
    ((partial reduce chg) (seq (:* p)))))

(defn patch-unchecked 
  "Apply a sequence of transformations to map 'm' specified by patch 'p' and
  return a new map with the result.  Patch changes are not checked for
  contextual validity."
  [m p]
  (-> m
    ((partial reduce del-unchecked) (seq (:- p)))
    ((partial reduce ins-unchecked) (seq (:+ p)))
    ((partial reduce chg-unchecked) (seq (:* p)))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Single Level Map Diff:

;; (diff
;;   {:a 1 :b 2 :c 3 :d 4}
;;   {:a 1 :b "2" :e 5})
;;
;;  =>  {:+ {[:e] 5},
;;       :- {[:d] 4, [:c] 3},
;;       :* {[:b] [2 "2"]}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nested Map Diff:

;; (diff
;;   {:a 1 :b 2 :c 3 :d {:D1 1 :D2 "2" :D3 5}}
;;   {:a {:A1 1}  :b 2 :c 3 :d {:D1 1 :D2 2 :D4 4}})
;;
;;  => {:+ {[:d :D4] 4},
;;      :- {[:d :D3] 5},
;;      :* {[:a] [1 {:A1 1}], [:d :D2] ["2" 2]}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Individual Patch Modifications:

;; (ins {:a 1} [[:e :E1] 5])
;;  =>  {:e {:E1 5}, :a 1}

;; (del {:a 1} [[:a] 1])
;;  =>  {}

;; (del {:a {:A1 1 :A2 2} :b {:B1 1}} [[:a :A2] 2])
;;  =>  {:b {:B1 1}, :a {:A1 1}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Map Diff and Patching:

;; (let [old {:a 1 :b 2 :c 3 :d {:D1 1 :D2 "2" :D3 5}}
;;       new {:a {:A1 1}  :b 2 :c 3 :d {:D1 1 :D2 2 :D4 4}}]
;;   (= (patch old (diff old new)) new))
;;
;;  => true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unchecked Patching:

;; (let [old {:a 1 :b 2 :c 3 :d {:D1 1 :D2 "2" :D3 5}}
;;       new {:a {:A1 1}  :b 2 :c 3 :d {:D1 1 :D2 2 :D4 4}}]
;;   (patch-unchecked {} (diff old new)))
;;
;;  => {:a {:A1 1}, :d {:D2 2, :D4 4}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


