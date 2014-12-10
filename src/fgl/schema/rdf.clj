(ns fgl.schema.rdf
  (:refer-clojure :exclude [seq list first rest type]))

(def property   :rdf/Property)
(def seq        :rdf/Seq)
(def bag        :rdf/Bag)
(def alt        :rdf/Alt)
(def list       :rdf/List)
(def statement  :rdf/Statement)

(def type       :rdf/type)
(def first      :rdf/first)
(def rest       :rdf/rest)
(def subject    :rdf/subject)
(def predicate  :rdf/predicate)
(def object     :rdf/object)
(def member     :rdf/member)
(def null       :rdf/nil)
(def value      :rdf/value)



