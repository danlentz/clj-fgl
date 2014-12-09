# clj-fgl

A Functional Graph Library for Clojure

## Usage

With Leiningen:

```clj
[danlentz/clj-fgl "0.1.0-SNAPSHOT"]
```

## Overview

`clj-fgl` is a _tuple store_ based functional graph library in
Clojure. It differs from traditional graph libraries in that it
maintains a strictly functional approach and immutable data
model. `clj-fgl` also features _functional indexing_, _generalized query_,
and _dynamic graph context_.

### The Tuple

First, let's get something straight.  When people talk about graphs
these days, they almost inevitably start talking about _nodes_.  These
people are "node centric" thinkers.  They view their data as a set of
"things" and some of these things may well have various properties.
Ok. This is a pretty normal way to think about things, and is a
tradition deeply established in Frames systems for knowledge
representation and well represented in the modern graph database
market with fine choices such as Neo4J.

There is another 



This _generalized query_ interface is expressed with a Clojure
protocol for a _Graph_.  The information in a
Graph is encoded as a series of assertions of relations among its
constituents. They are encoded as tuples:

    [subject predicate object]

There are many advantages to this approach, such as the ability to
persist and restore graphs, and the inherent parallelism this type of
computation can achieve using Clojures _reducers_ library with
fork/join multiprocessing. The most significant advantage, thopugh, is
that it provides the ability to represent and efficiently query
arbitrarily complex and dynamically extensible concepts of data.  We
may easily encode and index in unique ways by making assertions like
_"EVERYTHING" is 10 letters long_ or _"SOME" begins with 'S'_:

    ["EVERYTHING" :length 10]
    ["SOME"       \S       0]

But we may also arbitrarily extend our graph.  Say, for example, in
the future we want to extend our game to provide a clue to the word
being guessed that represented its possible "part of speech" (e.g.,
```:NOUN :VERB :ADJECTIVE :SLANG``` and so on).  We would simply extend our
tuple-store with the new assertions:

    ["CAT" :isa :NOUN]
    ["CAT" :isa :SLANG]

The interface and technique for adding a strategy that incorporates
this new type of information into its calculations would remain
exactly the same.  It would just then be able to perform additional
queries against the Graph to select for a given part of speech, or
dertermine the part of speech for a given word.  Because the data of a
Graph, tuples, are so generally defined, there can be clean separation
of concerns between implementation of strategy and mechanics of the
underlying indexing.  In fact, for inspiration about how various
shemata can be used to encode new kinds of concepts within a given
Graph, one might look to RDFS or OWL systems of description logic,
frequently used for machine learning and knowledge representation. 

Finally, the relations of a graph may be reified to express
meta-relations among the tuples themselves.  For example, one could
invent a new _predicate_, ```:confidence``` to express the ordinal
precidence among various numbered definitions of a word in some
dictionary:

    ["CAT" :def "A fuzzy creature..."    ]
    ["CAT" :def "Person; 'A cool cat'..."]
    [["CAT" :def "A fuzzy creature..."    ] :confidence 1]
    [["CAT" :def "Person; 'A cool cat'..."] :confidence 2]

Its also possible to fully embrace the meta and to include assertions
about the Graph itself or about other Graphs.

### Tuple-Store



fgl.triples> x
;; => #<Graph b041b850-0478-1196-9bc3-7831c1bbb832 (4 triples)>
fgl.triples> (triples x)
;; => #{[:a :b :d] [:a :b :c] [:a :a :a] [:a :a :B]}
fgl.triples> +null+
;; => #uuid "00000000-0000-0000-0000-000000000000"
fgl.triples> +NULL+
;; => #<Graph 00000000-0000-0000-0000-000000000000 (0 triples)>
fgl.triples> (graph +null+)
;; => #<Graph 00000000-0000-0000-0000-000000000000 (0 triples)>
fgl.triples> (graph nil)
;; => #<Graph 00000000-0000-0000-0000-000000000000 (0 triples)>
fgl.triples> (graph +NULL+)
;; => #<Graph 00000000-0000-0000-0000-000000000000 (0 triples)>
fgl.triples> (intern-graph (add-triples +NULL+ [:i :am :it]))
;; => #uuid "ff20a170-04fa-1196-9bc3-7831c1bbb832"
fgl.triples> (graph *1)
;; => #<Graph ff20a170-04fa-1196-9bc3-7831c1bbb832 (1 triples)>
fgl.triples> (add-triples +NULL+ [:i :am :it])
;; => #<Graph 203bd1e0-04fb-1196-9bc3-7831c1bbb832 (1 triples)>
fgl.triples> (graph (add-triples +NULL+ [:i :am :it]))
;; => #<Graph 29e14d60-04fb-1196-9bc3-7831c1bbb832 (1 triples)>
fgl.triples> (intern-graph (add-triples +NULL+ [:i :am :it]))
;; => #uuid "ff20a170-04fa-1196-9bc3-7831c1bbb832"


(id #{})
;; => #uuid "00000000-0000-0000-0000-000000000000"
(triples #{})
;; => #{}


Tuples are stored in Graphs which consist of a set of such tuples
combined with appropriate indexing to suport generalized query in the
form of another tuple that may use the special value _nil_ to
represent "wildcard" or some literal to select for that value.
Therefore, to enumerate all triples in a given Graph, g:

    (query g nil nil nil)

To find all words of length 3:

    (query g nil :length 3)


By composing multiple queries and performing aggregate operations to
create new graphs, we express arbitrarily complex data queries using a
well defined algebra of set operations. 

#### Indexing

A graph, under the hood, is indexed in some fashion so as to be
queried efficiently by corresponding specializations of the
```query``` multifunction.  This allows for alternative indexing
techniques to be introduced in the future without change to api.
There is a default, fully indexed graph implementation provided. It
is a _hierarchical index_ extending over various permutations of
```[s p o]``` -- _subject, predicate, object_. Please keep in mind
that the internal structre of a Graph's index should be considered an
implementation detail and not relied upon.  With that in mind, an
```[s p o]``` index might appear as follows:

    {[s p o] {
              "EVERYTHING" {
                            \E {
                                 0  ["EVERYTHING" \E 0]
                                 2  ["EVERYTHING" \E 2] }
                            \V {
                                 1  ["EVERYTHING" \V 1] }

                            ...}
                            
              "EVERYTIME"  {
                            \E {
                                 0  ["EVERYTIME"  \E 0]
                                 2  ["EVERYTIME"  \E 2] 
                                 8  ["EVERYTIME"  \E 8] }
                            ...}
              ...}
      ...}


You'll notice that when one fully descends the index, the last value
when one traverses the constituents of a given tuple is the tuple
itself.  There are a couple of thoughts behind this structure, the
first being the efficiency of holding the triple as a result of
successful query, rather than recreating it.  Additionally, this
allows for a symmetry of structure as nested key/value indices without
imposing that the deepest layer is instead a set.  Thus, the actual
schema of an ```[s p o]``` index is _subject, predicate, object, identity_:

    (s p o . i)

Likewise, an ```[p o s]``` index might look like:

    {[p o s] {
              \E {
                   0 {           
                       "EVERYTHING" ["EVERYTHING" \E 0]
                       "EVERYTIME"  ["EVERYTIME"  \E 0]
                       "EVERYONE"   ["EVERYONE"   \E 0]
                       "EVERYPLACE" ["EVERYPLACE" \E 0] 
                       "EVERYMAN"   ["EVERYMAN"   \E 0]
                       ...}
                   1 {
                       "TEAM"       ["TEAM"       \E 1]
                       "LEADER"     ["LEADER"     \E 1]
                       ...}
                   ...}
             ...}
      ...}

#### Context

Context refers to a default global state as may be in effect at some
point in program execution. It is a graph among a global collection
represented and indexed by UUID identifier.  Once a graph has been
interned in this index, the fully built graph structure will be stored
and associated with both its UUID identifier and the set of triples it
contains.  The operator ```intern-graph``` is used to instate a given
graph context, after which that graph will remain indexed accessible
by content.  Notice that after a given graph has been interned by its
use as a context, any query returning a graph with the same contents
returns the same, physical graph.  It is only indexed once.  Think of
this as a kind of _graph memoization_.  Or, in other words, graphs are
addressable by content.

A higher-level query protocol, ```select```, is used to perform
context-aware query.  ```select``` indirectly invokes the appropriate
query multifunction according to the class of the input graph and the
supplied constituents of a query triple.

	;;;
	;;; Identity and Context: examples.
	;;;

	;; (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
	;;   => #<Graph e9a62310-7238-1195-8101-7831c1bbb832 (2 triples)>

	;; (with-context #{[1 2 3]}
	;;   (query (graph *context*) 1 2 nil))
	;;   => #{[1 2 3]}

	;; (with-context (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
	;;   (triples (select (graph nil) [nil nil nil])))
	;;   => #{[4 5 6] [1 2 3]}

	;; (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
	;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 triples)>
	;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 triples)>
	;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 triples)>

	;; (with-context (select (graph #{[1 2 3] [4 5 6]}) [nil nil nil])
	;;   (select (graph nil) [nil nil nil]))
	;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 triples)>
	;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 triples)>
	;;   => #<Graph 0322eb40-723c-1195-8101-7831c1bbb832 (2 triples)>




## License

Copyright © 2014 Dan Lentz

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
