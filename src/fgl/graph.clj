(ns fgl.graph
  (:refer-clojure :exclude [])
  (:require [clojure.pprint :as pp])
  (:require [clojure.core.reducers :as r])
  (:require [fgl.util  :as util])
  (:require [fgl.diff  :as diff])
  (:require [clj-uuid  :as uuid])
  (:use     [clj-tuple]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce db (atom {}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Physical Data Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn node? [x]
  (or (nil? x) (uuid/uuid? x)))

(defn node
  ([]
     (uuid/v1))
  ([x]
     (uuid/the-uuid x)))

(defn edge [S P O]
  (tuple S P O))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuple Constituent Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn s [[S P O]]
  S)

(defn p [[S P O]]
  P)

(defn o [[S P O]]
  O)

(defn sp [[S P O]]
  (list S P))

(defn po [[S P O]]
  (list P O))

(defn so [[S P O]]
  (list S O))

(defn spo [[S P O]]
  (edge S P O))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph / GraphContainer Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Graph [id index edges])

(defprotocol GraphContainer
  (id       [this])
  (index    [this])
  (edges    [this]))

(extend-type Graph GraphContainer
             (id [this]
               (:id this))
             (index [this]
               (:indexes this))
             (edges [this]
               (:edges this)))


(defmethod print-method Graph [g ^java.io.Writer w]
  (.write w "#<Graph ")
  (.write w (str (id g)))
  (.write w " (")
  (.write w (str (count (edges g))))
  (.write w " edges)>"))

(extend-type clojure.lang.PersistentHashSet GraphContainer
             (id [this]
               (if-let [g (@db this)]
                 (id g)
                 (uuid/v4)))
             (index [this]
               (if-let [g (@db this)]
                 (index g)
                 {}))
             (edges [this]
               (assert (every? #(= 3 (count %)) this))
               this))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-subindex [edge-collection key1 key2 key3]
  (reduce (fn [i0 e]
            (let [i1 (or (i0 (key1 e)) {})
                  i2 (or (i1 (key2 e)) {})]
              (assoc i0
                (key1 e)
                (assoc i1
                  (key2 e)
                  (assoc i2
                    (key3 e)
                    e)))))
    {} edge-collection))


;;;;
;;; The following index implementation exploits clojure reducers
;;; concurrency.  It is included for reference and is not
;;; currently in use because in practice it provides no gains
;;; in performance
;;;;

;; (defn make-index [edges key1 key2 key3]
;;   (r/fold util/rmerge (fn
;;                         ([] {})
;;                         ([i0 triple]
;;                            (let [i0 (or i0 {})
;;                                  i1 (or (i0 (key1 triple)) {})
;;                                  i2 (or (i1 (key2 triple)) {})]
;;                              (assoc i0
;;                                (key1 triple)
;;                                (assoc i1
;;                                  (key2 triple)
;;                                  (assoc i2
;;                                    (key3 triple)
;;                                    triple))))))
;;     edges))

;;;;
;;; The following index implementation creates a set for the innermost
;;; index rather than a map.  It is included for reference and is not
;;; currently in use. 
;;;;

;; (defn make-index [edges key1 key2 key3]
;;   (reduce (fn [i0 triple]
;;             (let [i1 (or (i0 (key1 triple)) {})
;;                   i2 (or (i1 (key2 triple)) #{})]
;;               (assoc i0
;;                 (key1 triple)
;;                 (assoc i1
;;                   (key2 triple)
;;                   (conj i2 (key3 triple))))))
;;     {} edges))


(defn add-subindex [g key1 key2 key3]
  (->Graph (id g)
    (assoc (index g)
      [key1 key2 key3] (make-subindex (edges g) key1 key2 key3))
    (edges g)))


(defn make-graph
  ([tuples]
     (or (@db (set tuples))
       (make-graph (node) (edges (set tuples)))))
  ([id tuples]
     (assert (node? id))
     (-> (->Graph id {} (edges (set tuples)))
       (add-subindex s p o)
       (add-subindex p o s)
       (add-subindex o s p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GraphBuilder Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol GraphBuilder
  (graph [this]))


(extend-type Graph GraphBuilder
             (graph [this]
               this))

(extend-type clojure.lang.PersistentHashSet GraphBuilder
             (graph [this]
               (or (get @db this)                
                 (make-graph this))))

(extend-type clojure.lang.PersistentList GraphBuilder
             (graph [this]
               (graph (set this))))

(extend-type clojure.lang.PersistentVector GraphBuilder
             (graph [this]
               (graph (set this))))

(extend-type nil GraphBuilder
             (graph [_]
               (make-graph uuid/+null+ #{})))

(def +null+ uuid/+null+)
(def +NULL+ (graph nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental Graph (de)Contruction and (de)Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- index-edge [[S P O]]
  {[o s p] {:+ {[O S] {P (edge S P O)}}}
   [p o s] {:+ {[P O] {S (edge S P O)}}}
   [s p o] {:+ {[S P] {O (edge S P O)}}}})

(defn- add-edge [g [S P O]]
  (let [tups (conj (edges g) (tuple S P O))]
    (or (@db tups)
      (let [k (keys (indexes g))
            p (map (index-triple [S P O]) k)
            o (map (indexes g) k)
            n (zipmap k (map diff/patch-unchecked o p))]
        (->Graph (node) n tups)))))

(defn add-edges 
  "Efficiently add and index 0 or more edges to an existing graph,
  returning a new graph as the result."
  [g & more]
  (reduce add-triple g more))

;; (add-edges +NULL+ [:a :b :c] [:a :b :d] [:a :a :a] [:a :a :b])
;;  => #<Graph 80dc0041-0476-1196-9bc3-7831c1bbb832 (4 edges)>



(defn- deindex-triple [[S P O]]
  {[o s p] {:- {[O S] {P (tuple S P O)}}}
   [p o s] {:- {[P O] {S (tuple S P O)}}}
   [s p o] {:- {[S P] {O (tuple S P O)}}}})

(defn- del-triple [g [S P O]]
  (let [tups (disj (edges g) (tuple S P O))]
    (or (@db tups)
      (let [k (keys (indexes g))
            p (map (deindex-triple [S P O]) k)
            o (map (indexes g) k)
            n (zipmap k (map diff/patch-unchecked o p))]
        (->Graph (node) n tups)))))

(defn del-edges
  "Efficiently delete and deindex 0 or more edges from an existing graph,
  returning a new graph as the result."
  [g & more]
  (reduce del-triple g more))



(defn- merge-index [g1 g2]
  (let [g1  (graph g1)
        g2  (graph g2)
        k   (keys (indexes g1))
        ix1 (map (indexes g1) k)
        ix2 (map (indexes g2) k)
        ix  (map diff/merge* ix1 ix2)]
    (zipmap k ix)))


(defn- merge-graph [g1 g2]
  (let [tups (clojure.set/union (edges g1) (edges g2))]
    (or (@db tups)
      (->Graph (node) (merge-index (graph g1) (graph g2)) tups))))


(defn merge-graphs
  "Efficiently merge 0 or more indexed graphs, returning a new indexed graph"
  [& more]
  (if-not (seq (map graph more))
    (graph nil)
    (reduce merge-graph (first more) (rest more))))

(defn & 
  "Efficiently merge 0 or more indexed graphs, returning a new indexed graph"
  [& more]
  (apply merge-graphs more))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Database and Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn intern-graph [g]
  (let [x (@db (edges g))]
    (if x
      (id x)
      (util/returning (id g)
        (swap! db #(into % [[(id g) g] [(edges g) g]]))))))
  
(def ^{:dynamic true}    *context* (graph (intern-graph (graph nil))))

(defn current-context [] *context*)



(extend-type java.util.UUID GraphBuilder
             (graph [this]
               (or (get @db this)
                 +NULL+)))

(extend-type java.util.UUID GraphContainer
             (id [this]
               this)
             (indexes [this]
               (indexes (graph this)))
             (edges [this]
               (edges (graph this))))


(defmacro with-context [designator & body]
  `(binding [*context* (graph (intern-graph
                                (& *context* (graph ~designator))))]
     ~@body))

(defmacro with-null-context [& body]
  `(binding [*context* +NULL+]
     ~@body))

;; (with-context #{[:fido :isa :dog]}
;;   (with-context #{[:dog :isa :animal]}
;;     (select +NULL+ [nil :isa nil])))
;;
;;  => #<Graph 69b76850-05c5-1196-9bc3-7831c1bbb832 (2 edges)>

;; (with-context #{[:fido :isa :dog]}
;;   (with-context #{[:dog :isa :animal]}
;;     (with-null-context
;;       (select +NULL+ [nil :isa nil]))))
;;
;;  => #<Graph 00000000-0000-0000-0000-000000000000 (0 edges)>



;; (assert (= (graph *context*) (graph nil) (graph 0)))

;; (graph #{[1 2 3]})
;;  => #<Graph 2d1b48b0-723d-1195-8101-7831c1bbb832 (1 edges)>
;;  => #<Graph 270f47a0-723d-1195-8101-7831c1bbb832 (1 edges)>
;;  => #<Graph 257a01a0-723d-1195-8101-7831c1bbb832 (1 edges)>

;; (intern-graph (graph #{[1 2 3]}))
;;  => #uuid "6d863860-723d-1195-8101-7831c1bbb832"

;; (graph #{[1 2 3]})
;;  => #<Graph 6d863860-723d-1195-8101-7831c1bbb832 (1 edges)>
;;  => #<Graph 6d863860-723d-1195-8101-7831c1bbb832 (1 edges)>
;;  => #<Graph 6d863860-723d-1195-8101-7831c1bbb832 (1 edges)>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indexed Graph Query
;;
;; Implememnted as a multifunction rather than protocol to allow modular
;; separation of implmentation based on supplied constituents of query.
;; This, for example, might exploit various indexing capabilities of a
;; specific Graph implementation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn supplied-constituents [g subj pred obj] 
  (vec (conj (for [c [subj pred obj]]
               (not (nil? c)))
         (class g))))

(defmulti query supplied-constituents)

(defmethod query [Graph false false false] [graph subj pred obj]
  (edges graph))


(defmethod query [Graph true false false]  [graph subj pred obj]
  (let [i0 ((indexes graph) [s p o])
        i1 (get i0 subj)]
    (set (for [pred (keys i1)
               obj  (keys (get i1 pred))]
           (get-in i1 [pred obj])))))

(defmethod query [Graph false true false]  [graph subj pred obj]
  (let [i0 ((indexes graph) [p o s])
        i1 (get i0 pred)]
    (set (for [obj  (keys i1)
               subj (keys (get i1 obj))]
           (get-in i1 [obj subj])))))
 
(defmethod query [Graph false false true]  [graph subj pred obj]
  (let [i0 ((indexes graph) [o s p])
        i1 (get i0 obj)]
    (set (for [subj (keys i1)
               pred (keys (get i1 subj))]
           (get-in i1 [subj pred])))))

(defmethod query [Graph true true false]  [graph subj pred obj]
  (let [idx ((indexes graph) [s p o])]
    (set (for [obj (keys (get-in idx [subj pred]))]
           (tuple subj pred obj)))))
 
(defmethod query [Graph true false true]  [graph subj pred obj]
  (let [idx ((indexes graph) [o s p])]
    (set (for [pred (keys (get-in idx [obj subj]))]
           (tuple subj pred obj)))))
  
(defmethod query [Graph false true true]  [graph subj pred obj]
  (let [idx ((indexes graph) [p o s])]
    (set (for [subj (keys (get-in idx [pred obj]))]
           (tuple subj pred obj)))))
 
(defmethod query [Graph true true true]   [graph subj pred obj]
  (set (filter identity
         (vector ((edges graph) [subj pred obj])))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECT / GraphQuery Protocol (Context-Aware Graph Query)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol GraphQuery
  (select [this tup]))

(extend-type Graph GraphQuery
             (select [this tup]
               (graph
                 (let [subj (s tup)
                       pred (p tup)
                       obj  (o tup)]
                   (clojure.set/union
                     (query this subj pred obj)
                     (query (graph *context*) subj pred obj))))))

(extend-type nil GraphQuery
             (select [this tup]
               (graph nil)))

(extend-type java.lang.Long GraphQuery
             (select [this tup]
               (select (graph this) tup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Identity and Context: examples.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (select uuid/+null+ [1 2 3])
;;   => #<Graph 00000000-0000-0000-0000-000000000000 (0 edges)>

;; (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
;;   => #<Graph e9a62310-7238-1195-8101-7831c1bbb832 (2 edges)>    

;; (with-context #{[1 2 3]}
;;   (select +NULL+ [1 2 nil]))
;;
;;   => #<Graph 02a6e030-0536-1196-9bc3-7831c1bbb832 (1 edges)>

;; (with-context #{[1 2 3]}
;;   (edges (select +NULL+ [1 2 nil])))
;;
;;   => #{[1 2 3]}

;; (with-context (graph #{[1 2 4]})
;;   (with-context #{[1 2 5]} 
;;     (edges (select +NULL+ [1 2 nil]))))
;;
;; => #{[1 2 5] [1 2 4]}

;; (with-context (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
;;   (edges (select (graph nil) [nil nil nil])))
;;   => #{[4 5 6] [1 2 3]}

;; (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 edges)>
;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 edges)>
;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 edges)>

;; (with-context (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
;;   (select (graph nil) [nil nil nil]))
;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 edges)>
;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 edges)>
;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 edges)>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn graph-union [g & more]
  (graph
    (reduce clojure.set/union
      (map edges (conj more g)))))


(defn graph-intersection [g & more]
  (graph
    (reduce clojure.set/intersection
      (map edges (conj more g)))))


(defn graph-difference [g & more]
  (graph
    (reduce clojure.set/difference
      (map edges (conj more g)))))


(defn graph-equal? [g & more]
  (every? #(= (edges g) %) (map edges more)))

