(ns fgl.graph
  (:refer-clojure :exclude [])
  (:require [clojure.pprint :as pp])
  (:require [clojure.core.reducers :as r])
  (:require [fgl.util  :as util])
  (:require [fgl.diff  :as diff])
  (:require [fgl.edn   :as fedn])
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

(defn edge? [this]
  (and (coll? this) (= (count this) 3)))

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

(definterface IGraphStore
  (getId       [])
  (getIndex    [])
  (getEdges    []))

(declare select graph-equal? add-edges del-edges)

(defn- invoke-select [g thing]
  (if (edge? thing)
    (select g thing)
    (select g (edge thing nil nil))))


(deftype Graph [^java.util.UUID id
                index
                edges]

  IGraphStore

  (getId    [_]
    id)
  (getIndex [_]
    index)
  (getEdges [_]
    edges)

  java.lang.Object

  (hashCode [_]
    (bit-xor
      (.hashCode edges)
      (.hashCode index)))
  (equals [this other]
    (graph-equal? this other))
  (toString [_]
    (.toString edges))

  java.util.Set

  (size     [_]
    (count edges))
  (isEmpty  [_]
    (empty? edges))
  (iterator [_]
    (clojure.lang.SeqIterator. (seq edges)))
  (containsAll [_ things]
    (every? #(contains? edges %) things))
  
  clojure.lang.Seqable

  (seq [_]
    (map #(apply tuple %) (seq edges)))
   
  clojure.lang.IPersistentSet

  (equiv [this other]
    (graph-equal? this other))
  (count [_]
    (count edges))
  (empty [_]
    (@db uuid/+null+))
  (contains [_ edge-tuple]
    (contains? edges edge-tuple))
  (disjoin [this edge-tuple]
    (del-edges this edge-tuple))
  (cons [this edge-tuple]
    (add-edges this edge-tuple))

  clojure.lang.IFn

  (invoke [this vertex-or-generalized-edge]
    (invoke-select this vertex-or-generalized-edge))
  (invoke [this vertex-or-generalized-edge default]
    (let [result (invoke-select this vertex-or-generalized-edge)]
      (if (seq? (.getEdges result))
        result
        default))))
  
  
(defprotocol GraphContainer
  (id       [this])
  (index    [this])
  (edges    [this]))

(extend-type Graph GraphContainer
             (id [this]
               (.getId this))
             (index [this]
               (.getIndex this))
             (edges [this]
               (.getEdges this)))


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

(extend-type clojure.lang.LazySeq GraphBuilder
             (graph [this]
               (graph (vec this))))

(extend-type clojure.lang.PersistentVector GraphBuilder
             (graph [this]
               (graph (set this))))

(extend-type nil GraphBuilder
             (graph [_]
               (make-graph uuid/+null+ #{})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema Convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn schema [designator]
  (graph (fedn/edn-resource-value designator)))


;; (schema :rdfs)
;;
;;  => #<Graph 674ce640-085d-1196-a4d0-7831c1bbb832 (71 edges)>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental Graph (de)Contruction and (de)Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- index-edge [[S P O]]
  {[o s p] {:+ {[O S] {P (edge S P O)}}}
   [p o s] {:+ {[P O] {S (edge S P O)}}}
   [s p o] {:+ {[S P] {O (edge S P O)}}}})

(defn- add-edge [g [S P O]]
  (let [tups (conj (edges g) (edge S P O))]
    (or (@db tups)
      (let [k (keys (index g))
            p (map  (index-edge [S P O]) k)
            o (map  (index g) k)
            n (zipmap k (map diff/patch-unchecked o p))]
        (->Graph (node) n tups)))))

(defn add-edges 
  "Add and index 0 or more edges to an existing graph,
  returning a new graph as the result."
  [g & more]
  (reduce add-edge g more))


;; (add-edges +NULL+ [:a :b :c] [:a :b :d] [:a :a :a] [:a :a :b])
;;  => #<Graph 80dc0041-0476-1196-9bc3-7831c1bbb832 (4 edges)>



(defn- deindex-edge [[S P O]]
  {[o s p] {:- {[O S] {P (edge S P O)}}}
   [p o s] {:- {[P O] {S (edge S P O)}}}
   [s p o] {:- {[S P] {O (edge S P O)}}}})

(defn- del-edge [g [S P O]]
  (let [tups (disj (edges g) (edge S P O))]
    (or (@db tups)
      (let [k (keys (index g))
            p (map (deindex-edge [S P O]) k)
            o (map (index g) k)
            n (zipmap k (map diff/patch-unchecked o p))]
        (->Graph (node) n tups)))))

(defn del-edges
  "Delete and deindex 0 or more edges from an existing graph,
  returning a new graph as the result."
  [g & more]
  (reduce del-edge g more))

;; (del-edges
;;   (add-edges +NULL+ [:a :b :c] [:a :b :d] [:a :a :a] [:a :a :b])
;;   [:a :a :a]
;;   [:a :b :c])
;;
;;  => #<Graph 3aef12a0-05cc-1196-9bc3-7831c1bbb832 (2 edges)>


(defn- merge-index [g1 g2]
  (let [g1  (graph g1)
        g2  (graph g2)
        k   (keys (index g1))
        ix1 (map  (index g1) k)
        ix2 (map  (index g2) k)
        ix  (map diff/merge* ix1 ix2)]
    (zipmap k ix)))


(defn- merge-graph [g1 g2]
  (let [tups (clojure.set/union (edges g1) (edges g2))]
    (or (@db tups)
      (->Graph (node) (merge-index (graph g1) (graph g2)) tups))))


(defn merge-graphs
  "Merge 0 or more indexed graphs, returning a new indexed graph"
  [& more]
  (if-not (seq (map graph more))
    (graph nil)
    (reduce merge-graph (first more) (rest more))))

(defn & 
  "Merge 0 or more indexed graphs, returning a new indexed graph"
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


(def +null+ uuid/+null+)
(def +NULL+ (graph nil))
(intern-graph +NULL+)


(def ^{:dynamic true}    *context* +NULL+)

(defn current-context [] *context*)



(extend-type java.util.UUID GraphBuilder
             (graph [this]
               (or (get @db this)
                 +NULL+)))

(extend-type java.util.UUID GraphContainer
             (id [this]
               this)
             (index [this]
               (index (graph this)))
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

;;  => #<Graph 69b76850-05c5-1196-9bc3-7831c1bbb832 (2 edges)>

;; (with-context #{[:fido :isa :dog]}
;;   (with-context #{[:dog :isa :animal]}
;;     (with-null-context
;;       (select +NULL+ [nil :isa nil]))))
;;
;;  => #<Graph 00000000-0000-0000-0000-000000000000 (0 edges)>



;; (assert (= (graph *context*) (graph nil) (graph +null+)))

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

(defmethod query [Graph false false false] [g subj pred obj]
  (edges g))

(defmethod query [Graph true false false]  [g subj pred obj]
  (let [i0 ((index g) [s p o])
        i1 (get i0 subj)]
    (set (for [pred (keys i1)
               obj  (keys (get i1 pred))]
           (get-in i1 [pred obj])))))

(defmethod query [Graph false true false]  [g subj pred obj]
  (let [i0 ((index g) [p o s])
        i1 (get i0 pred)]
    (set (for [obj  (keys i1)
               subj (keys (get i1 obj))]
           (get-in i1 [obj subj])))))
 
(defmethod query [Graph false false true]  [g subj pred obj]
  (let [i0 ((index g) [o s p])
        i1 (get i0 obj)]
    (set (for [subj (keys i1)
               pred (keys (get i1 subj))]
           (get-in i1 [subj pred])))))

(defmethod query [Graph true true false]  [g subj pred obj]
  (let [idx ((index g) [s p o])]
    (set (for [obj (keys (get-in idx [subj pred]))]
           (edge subj pred obj)))))
 
(defmethod query [Graph true false true]  [g subj pred obj]
  (let [idx ((index g) [o s p])]
    (set (for [pred (keys (get-in idx [obj subj]))]
           (edge subj pred obj)))))
  
(defmethod query [Graph false true true]  [g subj pred obj]
  (let [idx ((index g) [p o s])]
    (set (for [subj (keys (get-in idx [pred obj]))]
           (edge subj pred obj)))))
 
(defmethod query [Graph true true true]   [g subj pred obj]
  (set (filter identity
         (vector ((edges g) [subj pred obj])))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECT / GraphQuery Protocol (Context-Aware Graph Query)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol GraphQuery
  (select [this [S P O]]))

(extend-type Graph GraphQuery
             (select [this [S P O]]
               (graph
                 (clojure.set/union
                   (query this S P O)
                     (query (graph (current-context)) S P O)))))

(extend-type nil GraphQuery
             (select [this [S P O]]
               (graph (query (graph (current-context)) S P O))))

(extend-type java.util.UUID GraphQuery
             (select [this [S P O]]
               (select (graph this) (edge S P O))))

(extend-type clojure.lang.PersistentHashSet GraphQuery
             (select [this [S P O]]
               (select (graph this) (edge S P O))))

(defn- collect-properties [m [k v]]
  (let [existing (m k)]
    (cond
      (nil? existing)       (assoc m k v)
      (not (set? existing)) (assoc m k #{existing v})
      true                  (assoc m k (conj existing v)))))
    
(defn entity
  "Return a map describing the properties of node 'x' in graph g
  in the current dynamic context."
  [g x]
  (reduce collect-properties {}
    (mapv (comp vec po) (edges (select g [x nil nil])))))


(defn entity-inverse
  "Return a map describing the properties of things for which 'x'
  is the object in graph g in the current dynamic context."
  [g x]
  (reduce collect-properties {}
    (mapv (comp vec reverse sp) (edges (select g [nil nil x])))))



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
;;

;;;
;; (def fido     (graph #{[:x :isa    :dog]
;;                        [:x :name "fido"]
;;                        [:x :tag    1234]}))
;;
;; (def doghouse (graph #{[:x :in   :texas]}))
;;
;; (with-context fido 
;;   (with-context rdfs
;;    (map first (edges (select nil [nil :rdf/type :rdfs/Class])))))
;;
;;   => (:rdfs/Container :rdfs/Class :rdfs/Literal :rdf/Bag :rdf/List
;;       :rdfs/ContainerMembershipProperty :rdfs/Datatype :rdfs/Resource
;;       :rdf/Statement :rdf/Alt :rdf/Seq :rdfs/Property)
;;
;;;
;; (entity fido :x)
;;
;;   => {:tag 1234, :name "fido", :isa :dog}
;;
;; (entity-inverse fido :dog)
;;
;;   => {:isa :x}
;;
;; (with-context doghouse
;;   (entity fido :x))
;;
;;   => {:tag 1234, :name "fido", :in :texas, :isa :dog}
;;
;; (with-context #{[:x :isa :animal]}
;;   (entity fido :x))
;;
;;   => {:name "fido", :tag 1234, :isa #{:dog :animal}}
;;
;; (entity-inverse (schema :rdfs) :rdfs/Class)
;;
;;   => {:rdfs/domain     :rdfs/subClassOf,
;;       :rdfs/subClassOf :rdfs/Datatype,
;;       :rdf/type        #{:rdfs/Property :rdf/List :rdf/Alt :rdfs/Container
;;                          :rdfs/Datatype :rdfs/ContainerMembershipProperty
;;                          :rdf/Statement :rdf/Bag :rdfs/Resource :rdfs/Class
;;                          :rdf/Seq :rdfs/Literal},
;;       :rdfs/range      #{:rdfs/domain :rdf/type :rdfs/subClassOf
;;                          :rdfs/range}}
;;;


;;;
;;  Graphs are themselves functions which look up their arguments
;; using context-sensitive select.  In other words, dynamic functions.
;; Graphs can be invoked on atomic literals, such or keywords or URL's,
;; to return a (context-enhanced) graph of all edges eminating from that
;; vertex.  Or, graphs can be invoked on a generalized edge query to
;; return the result graph, just as 'select'
;;
;;
;; (edges (fido :x))
;;
;;   => #{[:x :tag 1234] [:x :name "fido"] [:x :isa :dog]}
;;
;; (edges (fido [:x :isa nil]))
;;
;;   => #{[:x :isa :dog]}
;;
;; (with-context doghouse
;;   (fido :x))
;;
;;   => #<Graph e7e60e00-079d-1196-a4d0-7831c1bbb832 (4 edges)>
;;
;; (fido nil)
;;
;;   => #<Graph 537d9560-079f-1196-a4d0-7831c1bbb832 (3 edges)>
;;;

;;;
;;  Graphs work just like you would expect with many common Clojure
;;  functions by implementing many interfaces, such as
;;  clojure.lang.PersistentSet.
;;
;;
;; (seq fido)
;;   => ([:x :tag 1234] [:x :name "fido"] [:x :isa :dog])
;;
;; (first fido)
;;   => [:x :tag 1234]
;;
;; (rest fido)
;;   => ([:x :name "fido"] [:x :isa :dog])
;;
;; (count fido)
;;   => 3
;;
;; (= fido (graph (seq fido)))
;;   => true
;;
;; (conj fido [:x :said "woof!"])
;;   => #<Graph 4d7b1c50-07c2-1196-a4d0-7831c1bbb832 (4 edges)>
;;
;; (seq (conj fido [:x :said "woof!"]))
;;   => ([:x :said "woof!"] [:x :tag 1234] [:x :name "fido"] [:x :isa :dog])
;;
;; (contains? fido [:x :isa :dog])
;;   => true
;;
;; (contains? fido [:x :isa :cat])
;;   => false
;;
;; (map type fido)
;;   => (clj_tuple.Tuple3 clj_tuple.Tuple3 clj_tuple.Tuple3)
;;
;;;

;;;
;;
;; Mapping like function over sequential generalized edge queries:
;;
;; (mapcat fido [[nil :isa nil][nil :tag nil]])
;;   => ([:x :isa :dog] [:x :tag 1234])
;;
;;
;; Reduce the graph to the set of unique nodes:
;;
;; (reduce into #{} fido)
;;   => #{"fido" 1234 :isa :name :dog :x :tag}
;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Operations (basic implementation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn graph-nodes [g]
  (reduce into #{} g))

(defn graph-union [g & more]
  (apply & g more))


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


